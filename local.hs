{-# LANGUAGE OverloadedStrings #-}

import           Conduit ( Conduit, await, leftover, yield, liftIO
                         , (=$), ($$), ($$+), ($$++), ($$+-))
import           Control.Concurrent.Async (race_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Binary (decode)
import           Data.Binary.Get (runGet, getWord16be, getWord32le)
import           Data.Char (ord)
import           Data.Conduit.Network ( runTCPServer, runTCPClient
                                      , serverSettings, clientSettings
                                      , appSource, appSink)
import           Data.Monoid ((<>))
import           Data.IP (fromHostAddress, fromHostAddress6)
import           GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import           GHC.IO.Handle.FD (stdout)

import Shadowsocks.Encrypt (getEncDec)
import Shadowsocks.Util

initLocal :: Conduit ByteString IO ByteString
initLocal = do
    await
    yield "\x05\x00"
    await >>= maybe (return ()) (\request -> do
        let addrType = request `S.index` 3
            request' = S.drop 4 request
        (addr, payload, addrPort) <- case addrType of
            1 -> do     -- IPv4
                let (ip, rest) = S.splitAt 4 request'
                    addr = C.pack $ show $ fromHostAddress $ runGet getWord32le
                                                           $ L.fromStrict ip
                return (addr, ip, S.take 2 rest)
            3 -> do     -- domain name
                let addrLen = ord $ C.head request'
                    (domain, rest) = S.splitAt (addrLen + 1) request'
                return (S.tail domain, domain, S.take 2 rest)
            4 -> do     -- IPv6
                let (ip, rest) = S.splitAt 16 request'
                    addr = C.pack $ show $ fromHostAddress6 $ decode
                                                            $ L.fromStrict ip
                return (addr, ip, S.take 2 rest)
            _ -> error $ C.unpack $ S.snoc "Unknown address type: " addrType
        yield "\x05\x00\x00\x01\x00\x00\x00\x00\x10\x10"
        let addrToSend = S.singleton addrType <> payload <> addrPort
            port = runGet getWord16be $ L.fromStrict addrPort
        liftIO $ C.putStrLn $ "connecting " <> addr <> ":" <> C.pack (show port)
        leftover addrToSend)

initRemote :: (ByteString -> IO ByteString)
           -> Conduit ByteString IO ByteString
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
    let localSettings = serverSettings (local_port config) "*"
        remoteSettings = clientSettings (server_port config)
                                        (C.pack $ server config)
    C.putStrLn $ "starting local at " <> C.pack (show $ local_port config)
    runTCPServer localSettings $ \client -> do
        (encrypt, decrypt) <- getEncDec (method config) (password config)
        (clientSource, ()) <- appSource client $$+ initLocal =$ appSink client
        runTCPClient remoteSettings $ \appServer -> do
            (clientSource', ()) <-
                clientSource $$++ initRemote encrypt =$ appSink appServer
            race_
                (clientSource' $$+- cryptConduit encrypt =$ appSink appServer)
                (appSource appServer $$ cryptConduit decrypt =$ appSink client)
