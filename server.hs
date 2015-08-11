{-# LANGUAGE OverloadedStrings #-}

import           Conduit ( Conduit, Sink, await, awaitForever
                         , yield, liftIO, (=$), ($$), ($$+), ($$+-))
import           Control.Concurrent.Async (race_)
import           Control.Monad (void)
import           Data.Binary.Get (runGet, getWord16be)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Char (ord)
import           Data.Conduit.Network ( runTCPServer, runTCPClient
                                      , serverSettings, clientSettings
                                      , appSource, appSink)
import           Data.Monoid ((<>))
import           GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import           GHC.IO.Handle.FD (stdout)

import Shadowsocks.Encrypt (getEncDec)
import Shadowsocks.Util

initRemote :: (ByteString -> IO ByteString)
                   -> Sink ByteString IO (ByteString, Int)
initRemote decrypt = await >>= 
    maybe (error "Invalid request") (\encRequest -> do
        request <- liftIO $ decrypt encRequest
        let addrType = S.head request 
            request' = S.drop 1 request
        (addr, addrPort) <- case addrType of
            1 -> do     -- IPv4
                let (ip, rest) = S.splitAt 4 request'
                return (ip, S.take 2 rest)
            3 -> do     -- domain name
                let addrLen = ord $ C.head request'
                    (domain, rest) = S.splitAt (addrLen + 1) request'
                return (S.tail domain, S.take 2 rest)
            _ -> error "Unsupported yet."
        let port = fromIntegral $ runGet getWord16be $ L.fromStrict addrPort
        return (addr, port))

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
    let localSettings = serverSettings (server_port config) "*"
    C.putStrLn $ "starting server at " <> C.pack (show $ server_port config)
    runTCPServer localSettings $ \client -> do
        (encrypt, decrypt) <- getEncDec (method config) (password config)
        (clientSource, (host, port)) <-
            appSource client $$+ initRemote decrypt
        let remoteSettings = clientSettings port host
        C.putStrLn $ "connecting " <> host <> ":" <> C.pack (show port)
        runTCPClient remoteSettings $ \appServer -> race_
            (clientSource $$+- handleLocal decrypt =$ appSink appServer)
            (appSource appServer $$ handleRemote encrypt =$ appSink client)
