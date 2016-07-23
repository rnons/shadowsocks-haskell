{-# LANGUAGE OverloadedStrings #-}

import           Conduit (Sink, await, liftIO, (=$), ($$), ($$+), ($$+-))
import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (race_)
import           Control.Exception (throwIO)
import           Control.Monad (forever)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Conduit (catchC)
import           Data.Conduit.Network ( runTCPServer, runTCPClient
                                      , serverSettings, clientSettings
                                      , appSource, appSink, appSockAddr)
import           Data.Monoid ((<>))
import           Data.Streaming.Network(bindPortUDP)
import           GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import           GHC.IO.Handle.FD (stdout)
import           Network.Socket hiding (recvFrom)
import           Network.Socket.ByteString (recvFrom, sendAllTo)

import Shadowsocks.Encrypt (getEncDec)
import Shadowsocks.Util

initRemote :: (ByteString -> IO ByteString)
           -> Sink ByteString IO (ByteString, Int)
initRemote decrypt = await >>=
    maybe (liftIO $ throwIO NoRequestBody) (\encRequest -> do
        request <- liftIO $ decrypt encRequest
        case unpackRequest request of
            Right (_, destAddr, destPort, _) -> return (destAddr, destPort)
            Left addrType -> liftIO $ throwIO $ UnknownAddrType addrType
        )

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    config <- parseConfigOptions
    let localSettings = serverSettings (serverPort config) "*"
    C.putStrLn $ "starting server at " <> C.pack (show $ serverPort config)

    udpSocket <- bindPortUDP (serverPort config) "*"
    forkIO $ forever $ do
        (encRequest, sourceAddr) <- recvFrom udpSocket 65535
        forkIO $ do
            (encrypt, decrypt) <- getEncDec (method config) (password config)
            request <- decrypt encRequest
            let (_, destAddr, destPort, payload) =
                    either (error . show . UnknownAddrType)
                           id
                           (unpackRequest request)
            C.putStrLn $ "udp " <> destAddr <> ":" <> C.pack (show destPort)
            remoteAddr <- head <$>
                getAddrInfo Nothing (Just $ C.unpack destAddr)
                                    (Just $ show destPort)

            remote <- socket (addrFamily remoteAddr) Datagram defaultProtocol
            sendAllTo remote payload (addrAddress remoteAddr)
            (packet', sockAddr) <- recvFrom remote 65535
            let packed = packSockAddr sockAddr
            packet <- encrypt $ packed <> packet'
            sendAllTo udpSocket packet sourceAddr
            close remote

    runTCPServer localSettings $ \client -> do
        (encrypt, decrypt) <- getEncDec (method config) (password config)
        (clientSource, (host, port)) <-
            appSource client $$+
                initRemote decrypt `catchC` \e ->
                    error $ show (e :: SSException) <> " from "
                          <> showSockAddr (appSockAddr client)
        let remoteSettings = clientSettings port host
        C.putStrLn $ "connecting " <> host <> ":" <> C.pack (show port)
        runTCPClient remoteSettings $ \appServer -> race_
            (clientSource $$+- cryptConduit decrypt =$ appSink appServer)
            (appSource appServer $$ cryptConduit encrypt =$ appSink client)
