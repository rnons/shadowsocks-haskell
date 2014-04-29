{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (void)
import Data.Char (ord)
import Data.Binary.Get (runGet, getWord16be)
import Data.Binary.Put (runPut, putWord16be)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.IntMap.Strict (IntMap, fromList, (!))
import Data.Word (Word8)
import Data.Monoid ((<>))
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
-- import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Shadowsocks.Encrypt (getTable)

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just "7777")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    putStrLn "starting local at 7777"
    let table = getTable "abc"
        encryptTable = fromList $ zip [0..255] table
        decryptTable = fromList $ zip (map fromIntegral table) [0..255]
    print table
    sockHandler sock (encryptTable, decryptTable)
    close sock

sockHandler :: Socket -> (IntMap Word8, IntMap Word8) -> IO ()
sockHandler sock (encTable, decTable) = do
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
    sendAll remote $ encrypt addr_to_send
    C.putStrLn $ "connecting " <> addr <> ":" <> C.pack (show port)
    handleTCP conn remote encrypt decrypt
    sockHandler sock (encTable, decTable)
  where
    encrypt :: S.ByteString -> S.ByteString
    encrypt buf = S.pack $ map (\b -> encTable ! fromIntegral b) $ S.unpack buf
    decrypt :: S.ByteString -> S.ByteString
    decrypt buf = S.pack $ map (\b -> decTable ! fromIntegral b) $ S.unpack buf

getServer :: IO AddrInfo
getServer =
    fmap head $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                            Nothing (Just "8888")

localwait, remotewait :: MVar ()
localwait = unsafePerformIO newEmptyMVar
remotewait = unsafePerformIO newEmptyMVar

handleTCP :: Socket
          -> Socket
          -> (S.ByteString -> S.ByteString)
          -> (S.ByteString -> S.ByteString)
          -> IO ()
handleTCP conn remote encrypt decrypt = do
    forkIO handleLocal
    forkIO handleRemote
    void $ takeMVar localwait
    void $ takeMVar remotewait
    close conn
    close remote
  where
    handleLocal = do
        inData <- fmap encrypt $ recv conn 4096
        if S.null inData
            then putMVar localwait ()
            else sendAll remote inData >> handleLocal
    handleRemote = do
        inData <- fmap decrypt $ recv remote 4096
        if S.null inData
            then putMVar localwait ()
            else sendAll conn inData >> handleRemote
