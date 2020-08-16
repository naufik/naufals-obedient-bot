{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Discord.Comms (
  DiscordMessage(..),
  DiscordAttachment(..),

  -- General Functions
  withDefault,

  -- API Functions
  sendMessage,
  sendSimpleMessage,
  
  -- Gateway Functions
  createGatewayListener,
  sendGatewayHeartbeat,
  getGatewayData,
  sendGatewayRaw,
  getBotGateway
) where

-- TODO: Delete System.Environment.
import System.Environment
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Text.Encoding

import Network.WebSockets
import Network.HTTP.Req
import Data.Aeson
import Data.Aeson.Types

import Discord.Config

type DiscordChannelID = T.Text
type DiscordUserID = T.Text
type DiscordGuildID = T.Text

-- TODO: Create payload types.
data GatewaySignal = GatewaySignal Int (Maybe Integer) Value deriving Show

instance FromJSON GatewaySignal where
  parseJSON = withObject "GatewaySignal" $
    \v -> GatewaySignal <$> (v .: "op") <*> (v .:? "s") <*> (pure $ Object v)
  
data DiscordMessage = DiscordMessage T.Text (Maybe DiscordAttachment) 

instance ToJSON DiscordMessage where
  toJSON (DiscordMessage m a) = case a of 
    Just (DiscordAttachment mime url) -> (object ([ "content" .= m ,
                                          mime .= object [ "url" .= url ] ]))
    Nothing                           -> (object [ "content" .= m ])

instance FromJSON DiscordMessage where
  parseJSON = withObject "DiscordMessage" $
    \v -> DiscordMessage <$> (v .: "content") <*> (v .:? "$$$$")
  
data DiscordAttachment = DiscordAttachment T.Text T.Text deriving Show

-- BROKEN, DO NOT USE.
instance FromJSON DiscordAttachment where
  parseJSON = withObject "DiscordAttachment" $
    \v -> DiscordAttachment <$> (v .: "mime") <*> (v .: "url")

-- Constants and global helpers
withDefault :: (DiscordConfig -> IO a) -> IO a
withDefault func = loadDiscordConfig >>= func

apiBaseUrl :: Url Https
apiBaseUrl = https "discord.com" /: "api" /: "v6"

libData :: Value
libData = object [ "$os" .= ("linux" :: T.Text),
  "$browser" .= ("monads" :: T.Text),
  "$device" .= ("monads" :: T.Text) ]

-- Functions that deal with gateway.
-- TODO: change B.ByteString -> IO () to DiscordMessage -> IO ()
createGatewayListener :: DiscordConfig -> (DiscordConfig -> GatewaySignal -> ClientApp ()) -> ClientApp ()
createGatewayListener config processEvent conn =
  gatewayHello
  >>= (\(GatewaySignal i _ v, seqVar) ->
    (createHeartbeater 20000000 seqVar conn)
    >> sendGatewayIdentify config conn 
    -- start operation as usual.
    >> forever (receiveRoutine seqVar))
  where
    gatewayHello :: IO (GatewaySignal, MVar Integer)
    gatewayHello = getGatewayData conn
      >>= (return . fromMaybe (GatewaySignal 0 Nothing $ object []) . decodeStrict)
      >>= (\x -> newMVar 0 >>= pure . ((,) x))

    receiveRoutine :: MVar Integer -> IO ()
    receiveRoutine seqVar = void $ getGatewayData conn
      >>= pure . (\x -> (decodeStrict x :: Maybe GatewaySignal))
      >>= processMessage seqVar

    processMessage :: MVar Integer -> Maybe GatewaySignal -> IO ()
    processMessage seqVar (Just (GatewaySignal op seq pl)) =
      processEvent config (GatewaySignal op seq pl) conn
      >> case seq of  Just i -> (swapMVar seqVar i >> pure ())
                      Nothing  -> pure ()

createHeartbeater :: Int -> MVar Integer -> Connection -> IO ()
createHeartbeater interval seqVar conn = void $ sendGatewayHeartbeat Nothing conn
  >> (forkIO . forever) (
    readMVar seqVar >>=
    (\x -> sendGatewayHeartbeat (Just x) conn)
    >> threadDelay interval) 

-- Individual Send Functions
sendGatewayIdentify :: DiscordConfig -> Connection -> IO ()
sendGatewayIdentify config conn = sendTextData conn $ encode $
  object [ "op" .= (2 :: Integer), 
  "d" .= object [
    "token" .= (decodeUtf8 $ botAccessToken config),
    "properties" .= libData
  ]]

sendGatewayHeartbeat :: Maybe Integer -> Connection -> IO ()
sendGatewayHeartbeat Nothing conn = sendTextData conn $ encode $ object ["op" .= (1 :: Integer), "d" .= Null]
sendGatewayHeartbeat (Just seq) conn = sendTextData conn $ encode $ object ["op" .= (1 :: Integer), "d" .= seq]

getGatewayData :: Connection -> IO B.ByteString
getGatewayData = receiveData

sendGatewayRaw :: Connection -> B.ByteString -> IO ()
sendGatewayRaw = sendTextData

sendGatewayData :: ToJSON a => Connection -> a -> IO ()
sendGatewayData conn obj = sendTextData conn $ encode obj

-- Signal Processing
getSignalPayload :: GatewaySignal -> Value
getSignalPayload (GatewaySignal _ _ v) = v

-- Interacting with HTTP endpoints.
getBotGateway :: (MonadHttp m) => DiscordConfig -> m (JsonResponse Object)
getBotGateway dconf = req GET
  (apiBaseUrl /: "gateway" /: "bot")
  NoReqBody
  jsonResponse
  (header "Authorization" $ "Bot " <> botAccessToken dconf)

sendSimpleMessage :: (MonadHttp m) => DiscordConfig -> DiscordChannelID -> T.Text -> m LbsResponse
sendSimpleMessage dconf channel content = req POST
  (apiBaseUrl /: "channels" /: channel /: "messages")
  (ReqBodyJson . object $ ["content" .= content])
  lbsResponse
  (header "Authorization" $ "Bot " <> botAccessToken dconf)

sendMessage :: (MonadHttp m) => DiscordConfig -> DiscordChannelID -> DiscordMessage -> m LbsResponse
sendMessage dconf channel content = req POST
  (apiBaseUrl /: "channels" /: channel /: "messages")
  (ReqBodyJson content)
  lbsResponse
  (header "Authorization" $ "Bot " <> botAccessToken dconf)