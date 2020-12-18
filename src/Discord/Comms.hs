{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Discord.Comms (
  DiscordEvent(..),
  DiscordMessage(..),
  GatewaySignal(..),
  DiscordUser(..),

  signalToEvent,
  getEventType,
  -- General Functions
  withDefault,

  -- API Functions
  sendSimpleMessage,
  
  -- Gateway Functions
  createGatewayListener,
  getGatewayData,
  sendGatewayRaw,
  getBotGateway
) where

import Control.Concurrent.MVar
    ( swapMVar, newMVar, readMVar, MVar )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Monad ( join, forever, void )
import Data.Maybe ( fromMaybe )
import Data.Functor (void, (<&>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Text.Encoding
import Data.HashMap.Lazy
import Network.WebSockets
import Network.HTTP.Req
import Data.Aeson
import Data.Aeson.Types

import Discord.Config

-- TODO: Clean this UP.
type DiscordChannelID = T.Text
type DiscordUserID = T.Text
type DiscordGuildID = T.Text
type DiscordMessageID = T.Text

-- Constants and global helpers
withDefault :: (DiscordConfig -> IO a) -> IO a
withDefault func = loadDiscordConfig >>= func

discordApiBaseUrl :: Url Https
discordApiBaseUrl = https "discord.com" /: "api" /: "v6"

libData :: Value
libData = object [ "$os" .= ("linux" :: T.Text),
  "$browser" .= ("monads" :: T.Text),
  "$device" .= ("monads" :: T.Text) ]

-- Payloads and Events.
data GatewaySignal = GatewaySignal Int (Maybe Integer) Value deriving Show

instance FromJSON GatewaySignal where
  parseJSON = withObject "GatewaySignal" $
    \v -> GatewaySignal <$> (v .: "op") <*> (v .:? "s") <*> (pure $ Object v)

-- GatewaySignalTransformers
data DiscordEvent = 
  MessageCreate DiscordChannelID DiscordUser DiscordMessage |
  MessageReactionAdd DiscordChannelID DiscordUser DiscordMessageID DiscordEmoji |
  MessageReactionRemove DiscordChannelID DiscordUserID DiscordMessageID DiscordEmoji
  deriving (Show)

data DiscordUser = DiscordUser {
  userId            :: T.Text,
  username          :: T.Text,
  userNickname      :: Maybe T.Text,
  isBot             :: Bool,
  userDiscriminator :: T.Text
} deriving Show

instance FromJSON DiscordUser where
  parseJSON = withObject "DiscordUser" (\v -> do {
      nickname  <- v .: "nick"
    ; _user     <- (v .: "user") >>= parseFullUser
    ; pure $ _user { userNickname = nickname }
    }) <> withObject "DiscordUser" parseFullUser
    where
      parseFullUser :: Object -> Parser DiscordUser
      parseFullUser v = do {
          id <- v .: "id"
        ; un <- v .: "username"
        ; bot <- v .:? "bot"
        ; disc <- v .: "discriminator"
        ; pure $ DiscordUser {
          userId = id,
          username = un,
          userNickname = Nothing,
          isBot = Just True == bot,
          userDiscriminator = disc
        }}

data DiscordMessage = DiscordMessage {
  messageId       :: T.Text,
  messageContent  :: T.Text
} deriving (Show)

instance FromJSON DiscordMessage where
  parseJSON = withObject "DiscordMessage" $ \v -> 
    DiscordMessage <$> (v .: "id") <*> (v .: "content")

data DiscordEmoji = DiscordEmoji {
  emojiId         :: Maybe T.Text,
  emojiName       :: Maybe T.Text,
  animated        :: Bool
}

instance FromJSON DiscordEmoji where
  parseJSON = withObject "DiscordEmoji" $ \v ->
    DiscordEmoji <$> (v .: "id" <&> unNull) 
      <*> (v .: "name" <&> unNull)
      <*> (v .:? "animated" >>= \x -> pure $ x == Just True)
    where
      unNull :: Value -> Maybe T.Text
      unNull (String x) = Just x
      unNull _ = Nothing

instance Show DiscordEmoji where
  show emoji = case emojiName emoji of
    Just x  -> T.unpack $ ":" <> x <> ":"
    Nothing -> "null"

channel_ :: Object -> Parser DiscordChannelID
channel_ v = (v .: "d") >>= (.: "channel_id")

signalToEvent :: GatewaySignal -> Maybe DiscordEvent
signalToEvent (GatewaySignal 0 _ pl) = getEventType pl >>= flip payloadToEvent pl
signalToEvent (GatewaySignal _ _ _) = Nothing

getEventType :: Value -> Maybe T.Text
getEventType pl = join $ parseMaybe (withObject "Payload" $ \v -> v .:? "t") pl

payloadToEvent :: T.Text -> Value -> Maybe DiscordEvent
payloadToEvent "MESSAGE_CREATE" = parseMaybe (withObject "Payload" $ \v ->
  MessageCreate <$> channel_ v <*> ((v .: "d") >>= parseJSON . fullUser) <*> (v .: "d"))
payloadToEvent "MESSAGE_REACTION_ADD" = parseMaybe (withObject "Payload" $ \v ->
  MessageReactionAdd <$> channel_ v <*> ((v .: "d") >>= (.: "member")) <*> ((v .: "d") >>= (.: "message_id")) <*> ((v .: "d") >>= (.: "emoji")))
payloadToEvent "MESSAGE_REACTION_REMOVE" = parseMaybe (withObject "Payload" $ \v ->
  MessageReactionRemove <$> channel_ v <*> ((v .: "d") >>= (.: "user_id")) <*> ((v .: "d") >>= (.: "message_id")) <*> ((v .: "d") >>= (.: "emoji")))
payloadToEvent _ = const Nothing

fullUser :: HashMap T.Text Value -> Value
fullUser v = case v ! "member" of
  Object m -> Object $ m <> fromList [("user", v ! "author")]
  _ -> object []

-- Functions that deal with gateway.
-- TODO: change B.ByteString -> IO () to DiscordMessage -> IO ()
createGatewayListener :: DiscordConfig -> (DiscordConfig -> GatewaySignal -> ClientApp ()) -> ClientApp ()
createGatewayListener config processEvent conn =
  gatewayHello
  >>= (\(GatewaySignal i _ v, seqVar) ->
    createHeartbeater 20000000 seqVar conn
    >> sendGatewayIdentify config conn 
    -- start operation as usual.
    >> forever (receiveRoutine seqVar))
  where
    gatewayHello :: IO (GatewaySignal, MVar Integer)
    gatewayHello = getGatewayData conn
      >>= (return . fromMaybe (GatewaySignal 0 Nothing $ object []) . decodeStrict)
      >>= (\x -> newMVar 0 <&> (,) x)

    receiveRoutine :: MVar Integer -> IO ()
    receiveRoutine seqVar = void $ getGatewayData conn
      >>= processMessage seqVar . (\x -> (decodeStrict x :: Maybe GatewaySignal))

    processMessage :: MVar Integer -> Maybe GatewaySignal -> IO ()
    processMessage seqVar (Just (GatewaySignal op seq pl)) =
      processEvent config (GatewaySignal op seq pl) conn
      >> case seq of  Just i -> void $ swapMVar seqVar i
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
    "token" .= decodeUtf8 (botAccessToken config),
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
getBotGateway :: (MonadHttp m) => DiscordConfig -> m (JsonResponse Value)
getBotGateway dconf = req GET
  (discordApiBaseUrl /: "gateway" /: "bot")
  NoReqBody
  jsonResponse
  (header "Authorization" $ "Bot " <> botAccessToken dconf)

sendSimpleMessage :: (MonadHttp m) => DiscordConfig -> DiscordChannelID -> T.Text -> m (JsonResponse Value)
sendSimpleMessage dconf channel content = req POST
  (discordApiBaseUrl /: "channels" /: channel /: "messages")
  (ReqBodyJson . object $ ["content" .= content])
  jsonResponse
  (header "Authorization" $ "Bot " <> botAccessToken dconf)

sendSimpleReaction :: (MonadHttp m) => DiscordConfig -> DiscordChannelID -> DiscordMessageID -> T.Text -> m (JsonResponse Value)
sendSimpleReaction dconf channel message emoji = req PUT
  (discordApiBaseUrl /: "channels" /: channel /: "messages" /: message /: "emojis" /: emoji /: "@me")
  (ReqBodyJson . object  $ [])
  jsonResponse
  (header "Authorization" $ "Bot " <> botAccessToken dconf)