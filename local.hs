{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.Async (concurrently)
import qualified Control.Exception as E
import           Control.Monad (forever, void, unless)
import           Data.Char (ord)
import           Data.Binary.Get (runGet, getWord16be, getWord32le)
import           Data.Binary.Put (runPut, putWord16be)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Monoid ((<>))
import           GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import           GHC.IO.Handle.FD (stdout)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

import Shadowsocks.Encrypt (getEncDec)
import Shadowsocks.Util


main :: IO ()
main = withSocketsDo $ do
    config <- parseConfigOptions
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just $ show $ local_port config)
    let sockAddr = head addrinfos
    sock <- socket (addrFamily sockAddr) Stream defaultProtocol
    bindSocket sock (addrAddress sockAddr)
    listen sock 128
    serverAddr <- getServer (server config) (server_port config)
    hSetBuffering stdout NoBuffering

    C.hPutStrLn stdout $
        "starting local at " <> C.pack (show $ local_port config)
    serveForever sock config serverAddr

serveForever :: Socket -> Config -> AddrInfo -> IO ()
serveForever sock config serverAddr = forever $ do
    (conn, _) <- accept sock
    forkFinally (sockHandler conn config serverAddr)
                (\_ -> close conn)

sockHandler :: Socket -> Config -> AddrInfo -> IO ()
sockHandler conn config serverAddr =
    (do
        (encrypt, decrypt) <- getEncDec (method config) (password config)
        recv conn 262
        send conn "\x05\x00"
        msg <- recv conn 4
        let m = C.unpack msg

        (addr, addr_to_send') <- if ord (m !! 3) == 1
            then do
                addr_ip <- recv conn 4
                addr <- inet_ntoa $ runGet getWord32le $ L.fromStrict addr_ip
                return (C.pack addr, addr_ip)
            else do
                addr_len <- recv conn 1
                addr <- recv conn (ord $ head $ C.unpack addr_len)
                return (addr, addr_len <> addr)

        addr_port <- recv conn 2
        let addr_to_send = S.singleton (msg `S.index` 3) <> addr_to_send' <> addr_port
            port = runGet getWord16be $ L.fromStrict addr_port
        let reply = "\x05\x00\x00\x01" <> "\x00\x00\x00\x00" <>
                    L.toStrict (runPut $ putWord16be 2222)
        sendAll conn reply

        remote <- socket (addrFamily serverAddr) Stream defaultProtocol
        connect remote (addrAddress serverAddr)
        encrypt addr_to_send >>= sendAll remote
        C.putStrLn $ "connecting " <> addr <> ":" <> C.pack (show port)
        handleTCP conn remote encrypt decrypt)
        `E.catch` (\e -> do
                    close conn
                    void $ print (e :: E.SomeException))

getServer :: HostName -> Int -> IO AddrInfo
getServer hostname port =
    head <$> getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                         (Just hostname)
                         (Just $ show port)

handleTCP :: Socket
          -> Socket
          -> (ByteString -> IO ByteString)
          -> (ByteString -> IO ByteString)
          -> IO ()
handleTCP conn remote encrypt decrypt = do
    concurrently handleLocal handleRemote
    close remote
  where
    handleLocal = do
        inData <- recv conn 4096 >>= encrypt
        unless (S.null inData) $ sendAll remote inData >> handleLocal
    handleRemote = do
        inData <- recv remote 4096 >>= decrypt
        unless (S.null inData) $ sendAll conn inData >> handleRemote
