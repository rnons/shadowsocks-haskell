{-# LANGUAGE OverloadedStrings #-}

import           Conduit ( Conduit, await, awaitForever, leftover, yield
                         , liftIO, (=$), ($$), ($$+), ($$++), ($$+-))
import           Control.Concurrent.Async (concurrently)
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Binary.Get (runGet, getWord16be)
import           Data.Char (ord)
import           Data.Conduit.Network ( runTCPServer, runTCPClient
                                      , serverSettings, clientSettings
                                      , appSource, appSink)
import           Data.Monoid ((<>))
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
                return (ip, ip, S.take 2 rest)
            3 -> do     -- domain name
                let addrLen = ord $ C.head request'
                    (domain, rest) = S.splitAt (addrLen + 1) request'
                return (S.tail domain, domain, S.take 2 rest)
            _ -> error "Unsupported yet."
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

handleLocal :: (ByteString -> IO ByteString)
            -> Conduit ByteString IO ByteString
handleLocal encrypt = awaitForever $ \inData -> do
    enc <- liftIO $ encrypt inData
    yield enc

handleRemote :: (ByteString -> IO ByteString)
             -> Conduit ByteString IO ByteString
handleRemote decrypt = awaitForever $ \inData -> do
    dec <- liftIO $ decrypt inData
    yield dec

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
        (cont, ()) <- appSource client $$+ initLocal =$ appSink client
        runTCPClient remoteSettings $ \appServer -> do
            (lefto, ()) <- cont $$++ initRemote encrypt =$ appSink appServer
            void $ concurrently
                (lefto $$+- handleLocal encrypt =$ appSink appServer)
                (appSource appServer $$ handleRemote decrypt =$ appSink client)
