{-# LANGUAGE OverloadedStrings #-}
module Bot.Content (
    botIntents
) where

import Control.Monad
import Bot.Intent
import Discord.Comms
import Discord.Config
import Network.HTTP.Req
import qualified Data.Text as T
import Data.HashMap.Lazy
import Data.Maybe
import qualified Bot.Content.Poll as Poll
import qualified Bot.Content.Roll as Roll

botIntents :: IntentResolver
botIntents = intentProcessPrefix "%?" helpIntents (intentProcessPrefix "%" allIntents)

allIntents :: IntentResolver
allIntents = intentKeywords $ fromList [
      ("echo", echoIntent),
      ("about", aboutIntent),
      (Roll.prefix, Roll.rollIntent)
      ]

helpIntents :: IntentResolver
helpIntents = intentKeywords $ fromList [
      ("echo", echoHelpIntent),
      ("about", aboutHelpIntent),
      (Roll.prefix, Roll.helpIntent)
      ]

echoIntent :: IntentResolver
echoIntent = intent $ f
    where
        f conf e@(MessageCreate c _ m) conn = let mMod = (fromMaybe "Cannot echo empty message." $ T.stripPrefix "echo" $ messageContent m) in
            void $ runReq defaultHttpConfig $ sendSimpleMessage conf c mMod

echoHelpIntent :: IntentResolver
echoHelpIntent = intent f
  where
    f conf e@(MessageCreate c _ _) _ = void $ runReq defaultHttpConfig $ sendSimpleMessage conf c $
      "Command **echo**:\n"
      <> "Echoes the message you send after the `echo` command.\n"
      <> "Example Usage:\n"
      <> "> >> echo ping\n"

aboutIntent :: IntentResolver
aboutIntent = intent $ f
    where
        f conf e@(MessageCreate c _ _) conn = void $ runReq defaultHttpConfig $ sendSimpleMessage conf c aboutText
        aboutText = "An Unnamed Bot\n"
            <> "```\n"
            <> "Version: in-development 0.2.0\n"
            <> "Created for private use only.\n"
            <> "see: http://github.com/naufik/naufals-obedient-bot\n"
            <> "```\n"
            <> "Bot name is subject to change."

aboutHelpIntent :: IntentResolver
aboutHelpIntent = intent $ f
  where
    f conf e@(MessageCreate c _ _ ) conn = void $ runReq defaultHttpConfig $ sendSimpleMessage conf c $
      "Command **about**:\n"
      <> "Prints bot information."