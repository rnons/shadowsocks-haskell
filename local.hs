{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, forkFinally, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import qualified Control.Exception as E
import           Control.Monad (forever, void)
import           Data.Char (ord)
import           Data.Binary.Get (runGet, getWord16be)
import           Data.Binary.Put (runPut, putWord16be)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import           GHC.IO.Handle.FD (stdout)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

import Shadowsocks.Encrypt (getEncDec)
import Shadowsocks.Util (SSConfig(..), readConfig)

main :: IO ()
main = withSocketsDo $ do
    mconfig <- readConfig "config.json"
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (fmap (show . local_port) mconfig)
    let config = fromJust mconfig
        sockAddr = head addrinfos
    sock <- socket (addrFamily sockAddr) Stream defaultProtocol
    bindSocket sock (addrAddress sockAddr)
    listen sock 128
    serverAddr <- getServer (server config) (server_port config)
    hSetBuffering stdout NoBuffering

    C.hPutStrLn stdout $
        "starting local at " <> C.pack (show $ local_port config)
    mvar <- newEmptyMVar
    forkFinally (serveForever sock config serverAddr)
                (\_ -> putMVar mvar ())
    takeMVar mvar

serveForever :: Socket -> SSConfig -> AddrInfo -> IO ()
serveForever sock config serverAddr = forever $ do
    (conn, _) <- accept sock
    void $ forkIO $ sockHandler conn config serverAddr

sockHandler :: Socket -> SSConfig -> AddrInfo -> IO ()
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
                addr <- inet_addr (C.unpack addr_ip) >>= inet_ntoa
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
        wait <- newEmptyMVar
        handleTCP conn remote encrypt decrypt wait)
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
          -> MVar ()
          -> IO ()
handleTCP conn remote encrypt decrypt wait = do
    hdl1 <- forkIO handleLocal
    hdl2 <- forkIO handleRemote
    takeMVar wait
    killThread hdl1
    killThread hdl2
    close conn
    close remote
  where
    handleLocal = do
        inData <- recv conn 4096 >>= encrypt
        if S.null inData
            then putMVar wait ()
            else sendAll remote inData >> handleLocal
    handleRemote = do
        inData <- recv remote 4096 >>= decrypt
        if S.null inData
            then putMVar wait ()
            else sendAll conn inData >> handleRemote
