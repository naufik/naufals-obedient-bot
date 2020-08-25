{-# LANGUAGE OverloadedStrings #-}
module Bot.Content (
    botIntents
) where

import Control.Monad
import Bot.Intent
import Discord.Comms
import Discord.Config
import Network.HTTP.Req
import Data.Text as T
import Data.HashMap.Lazy

botIntents :: IntentResolver
botIntents = prefixed

prefixed :: IntentResolver
prefixed = intentProcessPrefix ">>" allIntents (\_ -> mempty)
    where
        allIntents :: IntentResolver'
        allIntents = intentKeywords' $ fromList [
            ("echo", echoIntent),
            ("about", aboutIntent)
            ]

echoIntent :: IntentResolver'
echoIntent = intent $ asSignalHandler f
    where
        f conf e@(MessageCreate c _ m) conn = let mMod = T.intercalate " " (Prelude.drop 2 $ T.words $ messageContent m) in
            void $ runReq defaultHttpConfig $ sendSimpleMessage conf c mMod

aboutIntent :: IntentResolver'
aboutIntent = intent $ asSignalHandler f
    where
        f conf e@(MessageCreate c _ _) conn = void $ runReq defaultHttpConfig $ sendSimpleMessage conf c aboutText
        aboutText = "An Untitled Naufik Project\n"
            <> "```\n"
            <> "version: 0.2.0\n"
            <> "idk what features should I add but hey it's here\n"
            <> "```"