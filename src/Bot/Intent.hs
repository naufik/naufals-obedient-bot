{-# LANGUAGE OverloadedStrings #-}

module Bot.Intent (
  createListenerFromIntents,

  BotOp(..),
  IntentResolver,
  IntentResolver',

  -- Intent Resolver Builders
  intent,
  intentSequential,
  intentKeywords,
  intentProcessPrefix,
  intentKeywords',

  -- Function Transformers
  asSignalHandler
) where

import Discord.Config
import Discord.Comms
import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Network.WebSockets
import Network.HTTP.Req

import Data.Maybe (listToMaybe)
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Lazy

type BotOp           = DiscordConfig -> GatewaySignal -> ClientApp ()
type IntentResolver  = GatewaySignal -> Maybe BotOp
type IntentResolver' = DiscordEvent -> Maybe BotOp

createListenerFromIntents :: IntentResolver -> BotOp
createListenerFromIntents getIntent conf sig = 
  case getIntent sig of
    Just botOp  -> void . forkIO . botOp conf sig
    Nothing     -> mempty

intent :: BotOp -> (a -> Maybe BotOp)
intent op _ = Just $ op

intentSequential :: [(GatewaySignal -> Bool, IntentResolver)] -> IntentResolver
intentSequential [] _ = Nothing
intentSequential ((test, op):intents) msg
  | test msg = op msg
  | otherwise = intentSequential intents msg

intentKeywords :: HashMap T.Text IntentResolver' -> IntentResolver
intentKeywords intentMap sig = signalToEvent sig >>= (\x -> getMessageKeyword x >>= (intentMap !?) >>= (flip ($)) x)

intentKeywords' :: HashMap T.Text IntentResolver' -> IntentResolver'
intentKeywords' intentMap e = getMessageKeyword e >>= (intentMap !?) >>= (flip ($)) e

getMessageKeyword :: DiscordEvent -> Maybe T.Text
getMessageKeyword (MessageCreate _ _ z) = (pure . T.words . messageContent) z >>= listToMaybe
getMessageKeyword _ = Nothing

intentProcessPrefix :: T.Text -> (IntentResolver') -> (IntentResolver) -> IntentResolver
intentProcessPrefix prefix catch loose sig = case signalToEvent sig >>= matchMessage of
  Just True -> signalToEvent sig >>= processSignal >>= catch
  _ -> loose sig
  where
    -- TODO: can be removed if you have some willingness to
    matchMessage :: DiscordEvent -> Maybe Bool
    matchMessage (MessageCreate _ u z) = Just $ and [(not . isBot) u, T.isPrefixOf prefix $ (T.strip . messageContent) z]
    matchMessage _ = Nothing

    processSignal :: DiscordEvent -> Maybe DiscordEvent
    processSignal (MessageCreate x y z) = (pure . messageContent) z 
      >>= T.stripPrefix (T.strip prefix)
      >>= pure . T.strip
      >>= \newText -> Just (MessageCreate x y z{ messageContent = newText })
    processSignal _ = Nothing

asSignalHandler :: (DiscordConfig -> DiscordEvent -> ClientApp()) -> BotOp
asSignalHandler (f) conf sig conn = case signalToEvent sig of
  Nothing     -> mempty
  Just event  -> f conf event conn