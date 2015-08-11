{-# LANGUAGE DeriveGeneric #-}
module Shadowsocks.Util
  ( Config (..)
  , parseConfigOptions
  , cryptConduit
  ) where

import           Conduit (Conduit, awaitForever, yield, liftIO)
import           Control.Monad (liftM)
import           Data.Aeson (decode', FromJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
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
