{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Configurator
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import Network.Wreq
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import System.Environment
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.SSL as Streams

main :: IO ()
main = do args <- getArgs
          config <- load [Required $ head args]
          email <- require config "email"
          password <- require config "password"
          startClient email password

data SessionInfo = SessionInfo { sToken :: B.ByteString
                               , sHost :: String
                               , sPath :: String }

-- | Starts an irccloud client with given login and password
startClient :: T.Text -> T.Text -> IO ()
startClient email password =
  do sessionInfo <- getSessionInfo email password
     runSSLClient (sHost sessionInfo)
                  443
                  (sPath sessionInfo)
                  WS.defaultConnectionOptions
                  [("Cookie", "session=" <> sToken sessionInfo),
                   ("Origin", "https://www.irccloud.com")]
                  (ircClient $ sToken sessionInfo)

-- | The websocket client for IrcCloud
ircClient :: B.ByteString -> WS.Connection -> IO ()
ircClient token conn = do putStrLn "Connected !"
                          WS.forkPingThread conn 5
                          forever $ WS.receiveData conn >>= handleMsg token

-- | Handle one websocket mesage
handleMsg :: B.ByteString -> B.ByteString -> IO ()
handleMsg token msg = case msg ^. key "type" . _String of
                       "oob_include" -> do let url = T.unpack $ msg ^. key "url" . _String
                                               opts = defaults & header "Cookie" .~ ["session=" <> token]
                                           r <- getWith opts $ "https://www.irccloud.com" ++ url
                                           --L.putStrLn $ r ^. responseBody
                                           putStrLn "Ignoring backlog"
                       "buffer_msg" -> let user = msg ^. key "from" . _String
                                           chan = msg ^. key "chan" . _String
                                           content = msg ^. key "msg" . _String
                                       in T.putStrLn $ chan <> " " <> user <> ": "<> content
                       _ -> B.putStrLn msg

-- | Connect to irccloud to get a session token and the websocket host and path
getSessionInfo :: T.Text -> T.Text -> IO SessionInfo
getSessionInfo email password =
  do r1 <- post "https://www.irccloud.com/chat/auth-formtoken" B.empty
     let formtoken = r1 ^. responseBody . key "token" . _String
         opts = defaults & header "x-auth-formtoken" .~  [encodeUtf8 formtoken]
     r2 <- postWith opts "https://www.irccloud.com/chat/login" [ "email" := email
                                                               , "password" := password
                                                               , "token" := formtoken ]
     return SessionInfo { sToken = encodeUtf8 $ r2 ^. responseBody . key "session" . _String
                        , sHost = T.unpack $ r2 ^. responseBody . key "websocket_host" . _String
                        , sPath = T.unpack $ r2 ^. responseBody . key "websocket_path" . _String}

-- | Run a secure websocket client
runSSLClient :: String -> Int -> String -> WS.ConnectionOptions -> WS.Headers -> WS.ClientApp a -> IO a
runSSLClient host port path opts heads app = SSL.withOpenSSL $
  do ctx <- SSL.context
     Streams.withConnection ctx host (fromIntegral port) $ \i o _ -> do
       stream <- WS.makeStream (Streams.read i)
                               (flip Streams.write o . liftA L.toStrict)
       WS.runClientWithStream stream host path opts heads app
