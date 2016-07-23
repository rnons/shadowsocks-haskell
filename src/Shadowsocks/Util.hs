{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shadowsocks.Util
  ( Config (..)
  , cryptConduit
  , parseConfigOptions
  , unpackRequest
  , packRequest
  , packSockAddr
  , showSockAddr
  , SSException(..)
  ) where

import           Conduit (Conduit, awaitForever, yield, liftIO)
import           Control.Exception (Exception, IOException, catch)
import           Data.Aeson (decode', FromJSON)
import           Data.Binary (decode)
import           Data.Binary.Get (runGet, getWord16be, getWord32le)
import           Data.Binary.Put (runPut, putWord16be, putWord32le)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Char (chr, ord)
import           Data.IP ( fromHostAddress, fromHostAddress6
                         , toHostAddress, toHostAddress6)
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Network.Socket (HostAddress, HostAddress6, SockAddr(..))
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)


data Config = Config
    { server        :: String
    , serverPort    :: Int
    , localPort     :: Int
    , password      :: String
    , timeout       :: Int
    , method        :: String
    } deriving (Show, Generic)

instance FromJSON Config

data Options = Options
    { _server        :: Maybe String
    , _serverPort    :: Maybe Int
    , _localPort     :: Maybe Int
    , _password      :: Maybe String
    , _method        :: Maybe String
    , _config        :: Maybe String
    } deriving (Show, Generic)

type AddrType = Int

data SSException = UnknownAddrType AddrType
                 | NoRequestBody
    deriving (Show, Typeable)

instance Exception SSException


nullConfig :: Config
nullConfig = Config "" 0 0 "" 0 ""

readConfig :: FilePath -> IO (Maybe Config)
readConfig fp = decode' <$> L.readFile fp

configOptions :: Parser Options
configOptions = Options
    <$> optional (strOption (long "server" <> short 's' <> metavar "ADDR"
              <> help "server address"))
    <*> optional (option auto (long "server-port" <> short 'p' <> metavar "PORT"
                <> help "server port"))
    <*> optional (option auto (long "local-port" <> short 'l' <> metavar "PORT"
                <> help "local port"))
    <*> optional (strOption (long "password" <> short 'k' <> metavar "PASSWORD"
              <> help "password"))
    <*> optional (strOption (long "method" <> short 'm' <> metavar "METHOD"
              <> help "encryption method, for example, aes-256-cfb"))
    <*> optional (strOption (long "config" <> short 'c' <> metavar "CONFIG"
               <> help "path to config file"))

parseConfigOptions :: IO Config
parseConfigOptions = do
    o <- execParser $ info (helper <*> configOptions)
                      (fullDesc <> header "shadowsocks - a fast tunnel proxy")
    let configFile = fromMaybe "config.json" (_config o)
    mconfig <- readConfig configFile `catch` \(e :: IOException) ->
        hPutStrLn stderr ("ERROR: Failed to load " <> show e) >> exitFailure
    let c = fromMaybe nullConfig mconfig
    return $ c { server = fromMaybe (server c) (_server o)
               , serverPort = fromMaybe (serverPort c) (_serverPort o)
               , localPort = fromMaybe (localPort c) (_localPort o)
               , password = fromMaybe (password c) (_password o)
               , method = fromMaybe (method c) (_method o)
               }

cryptConduit :: (ByteString -> IO ByteString)
             -> Conduit ByteString IO ByteString
cryptConduit crypt = awaitForever $ \input -> do
    output <- liftIO $ crypt input
    yield output

unpackRequest :: ByteString -> Either AddrType (AddrType, ByteString, Int, ByteString)
unpackRequest request = case addrType of
    1 ->        -- IPv4
        let (ip, rest) = S.splitAt 4 request'
            addr = C.pack $ show $ fromHostAddress $ runGet getWord32le
                                                   $ L.fromStrict ip
        in  Right (addrType, addr, unpackPort rest, S.drop 2 rest)
    3 ->        -- domain name
        let addrLen = ord $ C.head request'
            (domain, rest) = S.splitAt (addrLen + 1) request'
        in  Right (addrType, S.tail domain, unpackPort rest, S.drop 2 rest)
    4 ->        -- IPv6
        let (ip, rest) = S.splitAt 16 request'
            addr = C.pack $ show $ fromHostAddress6 $ decode
                                                    $ L.fromStrict ip
        in  Right (addrType, addr, unpackPort rest, S.drop 2 rest)
    _ -> Left addrType
  where
    addrType = fromIntegral $ S.head request
    request' = S.drop 1 request
    unpackPort = fromIntegral . runGet getWord16be . L.fromStrict . S.take 2

packPort :: Int -> ByteString
packPort = L.toStrict . runPut . putWord16be . fromIntegral

packInet :: HostAddress -> Int -> ByteString
packInet host port =
    "\x01" <> L.toStrict (runPut $ putWord32le host)
           <> packPort port

packInet6 :: HostAddress6 -> Int -> ByteString
packInet6 (h1, h2, h3, h4) port = 
    "\x04" <> L.toStrict (runPut (putWord32le h1)
               <> runPut (putWord32le h2)
               <> runPut (putWord32le h3)
               <> runPut (putWord32le h4))
           <> packPort port

packDomain :: ByteString -> Int -> ByteString
packDomain host port =
    "\x03" <> C.singleton (chr $ S.length host) <> host <> packPort port

packRequest :: Int -> ByteString -> Int -> ByteString
packRequest addrType destAddr destPort =
    case addrType of
        1 -> packInet (toHostAddress $ read $ C.unpack destAddr) destPort
        3 -> packDomain destAddr destPort
        4 -> packInet6 (toHostAddress6 $ read $ C.unpack destAddr) destPort
        _ -> error $ "Unknown address type: " <> show addrType

packSockAddr :: SockAddr -> ByteString
packSockAddr addr =
    case addr of
        SockAddrInet port host -> packInet host $ fromIntegral port
        SockAddrInet6 port _ host _ -> packInet6 host $ fromIntegral port
        _ -> error "unix socket is not supported"

showSockAddr :: SockAddr -> String
showSockAddr addr =
    case addr of
        SockAddrInet port host ->
            show (fromHostAddress host) <> ":" <> show port
        SockAddrInet6 port _ host _ ->
            show (fromHostAddress6 host) <> ":" <> show port
        _ -> error "unix socket is not supported"
