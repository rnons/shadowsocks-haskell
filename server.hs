{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import qualified Control.Exception as E
import Control.Monad (void, forever)
import Data.Char (ord)
import Data.Binary.Get (runGet, getWord16be)
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
                 Nothing (Just "8888")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    hSetBuffering stdout NoBuffering
    C.hPutStrLn stdout "starting server at 8888"
    (encrypt, decrypt) <- getTableEncDec ""
    mvar <- newEmptyMVar
    forkFinally (sockHandler sock encrypt decrypt) (\_ -> putMVar mvar ())
    takeMVar mvar

sockHandler :: Socket
            -> (ByteString -> IO ByteString)
            -> (ByteString -> IO ByteString)
            -> IO ()
sockHandler sock encrypt decrypt = forever $
    (do
        (conn, _) <- accept sock
        addrType <- recv conn 1 >>= decrypt

        addr <- if ord (head $ C.unpack addrType) == 1
            then do
                addr_ip <- recv conn 4 >>= decrypt
                inet_addr (C.unpack addr_ip) >>= inet_ntoa
            else do
                addr_len <- recv conn 1 >>= decrypt
                addr <- recv conn (ord $ head $ C.unpack addr_len) >>= decrypt
                return $ C.unpack addr

        addr_port <- recv conn 2 >>= decrypt
        let port = runGet getWord16be $ L.fromStrict addr_port

        remoteAddr <- fmap head $ 
            getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                        (Just addr)
                        (Just $ show port)
        remote <- socket (addrFamily remoteAddr) Stream defaultProtocol
        connect remote (addrAddress remoteAddr)
        putStrLn $ "connecting " <> addr <> ":" <> show port
        localwait <- newEmptyMVar
        remotewait <- newEmptyMVar
        void $ forkIO $ handleTCP conn remote encrypt decrypt localwait remotewait)
        `E.catch` (\e -> void $ print (e :: E.SomeException))

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
        inData <- recv conn 4096 >>= decrypt
        if S.null inData
            then putMVar localwait ()
            else sendAll remote inData >> handleLocal
    handleRemote = do
        inData <- recv remote 4096 >>= encrypt
        if S.null inData
            then putMVar localwait ()
            else sendAll conn inData >> handleRemote
