{-# LANGUAGE OverloadedStrings #-}

module Bot.Intent (
  createListenerFromIntents,

  BotOp(..),
  IntentResolver,

  -- Intent Resolver Builders
  intent,
  intentSequential,
  intentProcessPrefix,
  intentKeywords
) where

import Discord.Config
import Discord.Comms
import qualified Data.Text as T
import Network.WebSockets
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.HashMap.Lazy

type RootBotOp       = DiscordConfig -> GatewaySignal -> ClientApp ()
type BotOp           = DiscordConfig -> DiscordEvent -> ClientApp ()
type IntentResolver  = DiscordEvent -> Maybe BotOp

createListenerFromIntents :: IntentResolver -> RootBotOp
createListenerFromIntents getIntent conf sig = fromMaybe mempty $ do {
    ev    <- signalToEvent sig
  ; botOp <- getIntent ev
  ; pure $ botOp conf ev
  }

intent :: BotOp -> (DiscordEvent -> Maybe BotOp)
intent op _ = Just op

intentSequential :: [(DiscordEvent -> Bool, IntentResolver)] -> IntentResolver
intentSequential [] _ = Nothing
intentSequential ((test, op):intents) msg
  | test msg = op msg
  | otherwise = intentSequential intents msg

intentKeywords :: HashMap T.Text IntentResolver -> IntentResolver
intentKeywords intentMap e = getMessageKeyword e >>= (intentMap !?) >>= flip ($) e

getMessageKeyword :: DiscordEvent -> Maybe T.Text
getMessageKeyword (MessageCreate _ _ z) = (pure . T.words . messageContent) z >>= listToMaybe
getMessageKeyword _ = Nothing

intentProcessPrefix :: T.Text -> IntentResolver -> IntentResolver -> IntentResolver
intentProcessPrefix prefix catch loose ev = case matchMessage ev of
  Just True -> processSignal ev >>= catch >>= pure . unprefixedCatch
  _ -> loose ev
  where
    -- TODO: can be removed if you have some willingness to
    matchMessage :: DiscordEvent -> Maybe Bool
    matchMessage (MessageCreate _ u z) = Just $ and [(not . isBot) u, T.isPrefixOf prefix $ (messageContent) z]
    matchMessage _ = Nothing

    processSignal :: DiscordEvent -> Maybe DiscordEvent
    processSignal (MessageCreate x y z) = (pure . messageContent) z 
      >>= T.stripPrefix (T.strip prefix)
      >>= pure . T.strip
      >>= \newText -> Just (MessageCreate x y z{ messageContent = newText })
    processSignal _ = Nothing

    unprefixedCatch :: BotOp -> BotOp
    unprefixedCatch catch conf ev conn  = case processSignal ev of
      Just x  -> catch conf x conn
      Nothing -> catch conf ev conn