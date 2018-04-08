{-# LANGUAGE OverloadedStrings #-}

import           Conduit                  (ConduitT, await, connect, leftover,
                                           liftIO, yield, ($$+), ($$++), ($$+-),
                                           (.|))
import           Control.Concurrent.Async (race_)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as C
import           Data.Conduit.Network     (appSink, appSource, clientSettings,
                                           runTCPClient, runTCPServer,
                                           serverSettings)
import           Data.Monoid              ((<>))
import           GHC.IO.Handle            (BufferMode (NoBuffering),
                                           hSetBuffering)
import           GHC.IO.Handle.FD         (stdout)

import           Shadowsocks.Encrypt      (getEncDec)
import           Shadowsocks.Util

initLocal :: ConduitT ByteString ByteString IO ()
initLocal = do
    await
    yield "\x05\x00"
    await >>= maybe (return ()) (\request -> do
        let (addrType, destAddr, destPort, _) =
                either (error . show . UnknownAddrType)
                       id
                       (unpackRequest $ S.drop 3 request)
            packed = packRequest addrType destAddr destPort
        yield "\x05\x00\x00\x01\x00\x00\x00\x00\x10\x10"
        liftIO $ C.putStrLn $ "connecting " <> destAddr
                                            <> ":" <> C.pack (show destPort)
        leftover packed)

initRemote :: (ByteString -> IO ByteString)
           -> ConduitT ByteString ByteString IO ()
initRemote encrypt = do
    mAddrToSend <- await
    case mAddrToSend of
        Just addrToSend -> do
            enc <- liftIO $ encrypt addrToSend
            yield enc
        Nothing -> return ()

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    config <- parseConfigOptions
    let localSettings = serverSettings (localPort config) "*"
        remoteSettings = clientSettings (serverPort config)
                                        (C.pack $ server config)
    C.putStrLn $ "starting local at " <> C.pack (show $ localPort config)
    runTCPServer localSettings $ \client -> do
        (encrypt, decrypt) <- getEncDec (method config) (password config)
        (clientSource, ()) <- appSource client $$+ initLocal .| appSink client
        runTCPClient remoteSettings $ \appServer -> do
            (clientSource', ()) <-
                clientSource $$++ initRemote encrypt .| appSink appServer
            race_
                (clientSource' $$+- cryptConduit encrypt .| appSink appServer)
                (appSource appServer `Conduit.connect`
                    (cryptConduit decrypt .| appSink client))
