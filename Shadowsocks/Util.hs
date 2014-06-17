{-# LANGUAGE DeriveGeneric #-}
module Shadowsocks.Util
  ( Config (..)
  , parseConfigOptions
  ) where

import           Control.Monad (liftM)
import           Data.Aeson (decode', FromJSON)
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Options.Applicative
import           Options.Applicative.Types (ReadM(ReadM))

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

optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m =
    nullOption $ value Nothing <> reader (success . str) <> m
  where
    success = ReadM . Right

maybeOpt :: Read a => Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOpt m =
    nullOption $ value Nothing <> reader (success . auto) <> m
  where
    success = ReadM . Right

configOptions :: Parser Options
configOptions = Options
    <$> optStr (long "server" <> short 's' <> metavar "ADDR"
               <> help "server address")
    <*> maybeOpt (long "server-port" <> short 'p' <> metavar "PORT"
                 <> help "server port")
    <*> maybeOpt (long "local-port" <> short 'l' <> metavar "PORT"
                 <> help "local port")
    <*> optStr (long "password" <> short 'k' <> metavar "PASSWORD"
               <> help "password")
    <*> optStr (long "method" <> short 'm' <> metavar "METHOD"
               <> help "encryption method, for example, aes-256-cfb")
    <*> optStr (long "config" <> short 'c' <> metavar "CONFIG"
               <> help "path to config file")

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
