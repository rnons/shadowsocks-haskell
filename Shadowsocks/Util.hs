{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Shadowsocks.Util
  ( Config (..)
  , cryptConduit
  , parseConfigOptions
  , unpackRequest
  ) where

import           Conduit (Conduit, awaitForever, yield, liftIO)
import           Control.Monad (liftM)
import           Data.Aeson (decode', FromJSON)
import           Data.Binary (decode)
import           Data.Binary.Get (runGet, getWord16be, getWord32le)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import           Data.Char (ord)
import           Data.IP (fromHostAddress, fromHostAddress6)
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Options.Applicative

data Config = Config
    { server        :: String
    , server_port   :: Int
    , local_port    :: Int
    , password      :: String
    , timeout       :: Int
    , method        :: String
    } deriving (Show, Generic)

instance FromJSON Config

data Options = Options
    { _server        :: Maybe String
    , _server_port   :: Maybe Int
    , _local_port    :: Maybe Int
    , _password      :: Maybe String
    , _method        :: Maybe String
    , _config        :: Maybe String
    } deriving (Show, Generic)

nullConfig :: Config
nullConfig = Config "" 0 0 "" 0 ""

readConfig :: FilePath -> IO (Maybe Config)
readConfig fp = liftM decode' $ L.readFile fp

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
    mconfig <- readConfig configFile
    let c = fromMaybe nullConfig mconfig
    return $ c { server = fromMaybe (server c) (_server o)
               , server_port = fromMaybe (server_port c) (_server_port o)
               , local_port = fromMaybe (local_port c) (_local_port o)
               , password = fromMaybe (password c) (_password o)
               , method = fromMaybe (method c) (_method o)
               }

cryptConduit :: (ByteString -> IO ByteString)
             -> Conduit ByteString IO ByteString
cryptConduit crypt = awaitForever $ \input -> do
    output <- liftIO $ crypt input
    yield output

unpackRequest :: ByteString -> (Int, ByteString, Int, ByteString)
unpackRequest request = (addrType, destAddr, destPort, payload)
  where
    addrType = fromIntegral $ S.head request
    request' = S.drop 1 request
    (destAddr, port, payload) = case addrType of
        1 ->        -- IPv4
            let (ip, rest) = S.splitAt 4 request'
                addr = C.pack $ show $ fromHostAddress $ runGet getWord32le
                                                       $ L.fromStrict ip
            in  (addr, S.take 2 rest, S.drop 2 rest)
        3 ->        -- domain name
            let addrLen = ord $ C.head request'
                (domain, rest) = S.splitAt (addrLen + 1) request'
            in  (S.tail domain, S.take 2 rest, S.drop 2 rest)
        4 ->        -- IPv6
            let (ip, rest) = S.splitAt 16 request'
                addr = C.pack $ show $ fromHostAddress6 $ decode
                                                        $ L.fromStrict ip
            in  (addr, S.take 2 rest, S.drop 2 rest)
        _ -> error $ "Unknown address type: " <> show addrType
    destPort = fromIntegral $ runGet getWord16be $ L.fromStrict port
