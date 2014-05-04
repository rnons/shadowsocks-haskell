{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Char (ord)
import Data.Binary.Get (runGet, getWord16be)
import Data.Binary.Put (runPut, putWord16be)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Monoid ((<>))
import GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import GHC.IO.Handle.FD (stdout)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
-- import System.Environment (getArgs)

import Shadowsocks.Encrypt (getTableEncDec, getEncDec)

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just "7777")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    hSetBuffering stdout NoBuffering
    C.hPutStrLn stdout "starting server at 7777"
    (encrypt, decrypt) <- getTableEncDec ""
    sockHandler sock encrypt decrypt
    close sock

sockHandler :: Socket
            -> (ByteString -> IO ByteString)
            -> (ByteString -> IO ByteString)
            -> IO ()
sockHandler sock encrypt decrypt = do
    (do
        (conn, _) <- accept sock
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

        serverAddr <- getServer
        remote <- socket (addrFamily serverAddr) Stream defaultProtocol
        connect remote (addrAddress serverAddr)
        encrypt addr_to_send >>= sendAll remote
        C.putStrLn $ "connecting " <> addr <> ":" <> C.pack (show port)
        localwait <- newEmptyMVar
        remotewait <- newEmptyMVar
        void $ forkIO $ handleTCP conn remote encrypt decrypt localwait remotewait)
        `E.catch` (\e -> void $ print (e :: E.SomeException))
    sockHandler sock encrypt decrypt

getServer :: IO AddrInfo
getServer =
    fmap head $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                            Nothing (Just "8888")

handleTCP :: Socket
          -> Socket
          -> (ByteString -> IO ByteString)
          -> (ByteString -> IO ByteString)
          -> MVar ()
          -> MVar ()
          -> IO ()
handleTCP conn remote encrypt decrypt localwait remotewait = do
    forkIO handleLocal
    forkIO handleRemote
    void $ takeMVar localwait
    void $ takeMVar remotewait
    close conn
    close remote
  where
    handleLocal = do
        inData <- recv conn 4096 >>= encrypt
        if S.null inData
            then putMVar localwait ()
            else sendAll remote inData >> handleLocal
    handleRemote = do
        inData <- recv remote 4096 >>= decrypt
        if S.null inData
            then putMVar localwait ()
            else sendAll conn inData >> handleRemote
