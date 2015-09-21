{-# LANGUAGE OverloadedStrings #-}

import           Conduit (Sink, await, liftIO, (=$), ($$), ($$+), ($$+-))
import           Control.Concurrent.Async (race_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
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
        let (_, destAddr, destPort, _) = unpackRequest request
        return (destAddr, destPort))

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
            (clientSource $$+- cryptConduit decrypt =$ appSink appServer)
            (appSource appServer $$ cryptConduit encrypt =$ appSink client)
