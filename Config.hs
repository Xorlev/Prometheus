module Config
    (Config(..)
    ,getConfig)
    where

import Data.Word
import Data.ConfigFile
import Network (PortNumber)

{-        let cp = forceEither val
        server = forceEither $ get cp "" "server"
        let port = fromIntegral $ forceEither $ get cp "" "port"-}

data Config = Config {
      configServer :: String
    , configPort :: PortNumber
    , configChannel :: String
    , configNick :: String
    } deriving (Show)

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        server <- get c "" "server"
        port <- get c "" "port"
        channel <- get c "" "channel"
        nick <- get c "" "nick"
        return Config { configServer = server
                      , configPort = fromIntegral (port::Word16)
                      , configChannel = channel 
                      , configNick = nick
                      }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config