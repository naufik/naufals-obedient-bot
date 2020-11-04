{-# LANGUAGE OverloadedStrings #-}

module Bot.Content.Poll (
  prefix,
  createPollIntent,
  helpIntent
) where
  
import Discord.Comms
import Bot.Intent
import qualified Data.Text as T
import Network.HTTP.Req
import Control.Monad
import Data.Maybe

data PollCreate = Poll { title :: T.Text, options :: [T.Text] }

prefix :: T.Text
prefix = "poll"

pollIntents :: IntentResolver
pollIntents = intentSequential [
    (testCreate, createPollIntent),
    (testVote, createPollIntent)
  ]
  where
    testCreate ev@(MessageCreate _ user msg) =
      and [
        not $ isBot user,
        T.isPrefixOf (prefix) (messageContent msg)
      ]
    testCreate _ = False

    testVote ev@(MessageReactionAdd _ user m e) = 
      and [
        not $ isBot user
      ]

createPollIntent :: IntentResolver
createPollIntent = intent createPoll'

createPoll' :: BotOp
createPoll' conf (MessageCreate c user m) _ =
  fromMaybe mempty $ parsePoll (messageContent m) >>= \poll -> 
    pure $
      store poll >> 
      pollMsg user poll
  where
    parsePoll :: T.Text -> Maybe PollCreate
    parsePoll msg = do {
      args <- T.stripPrefix "poll" msg >>= pure . map T.strip . T.splitOn "\n"
    ; pure $ Poll {
        title = head args,
        options = drop 1 args
      }
    }

    -- mock function change later.
    store :: PollCreate -> IO ()
    store poll = print $ title poll

    pollMsg :: DiscordUser -> PollCreate -> IO ()
    pollMsg user poll = (runReq defaultHttpConfig $ sendSimpleMessage conf c (makeMsg user poll)) >>= putStrLn . show

    makeMsg :: DiscordUser -> PollCreate -> T.Text
    makeMsg user poll = "Poll: **" <> title poll <> "**\n" <> "by user " <> username user <> "\n\n"
     <> "> Select from the options below by reacting: \n"
     <> T.intercalate "\n" (map (\(a,b) -> a <> " " <> b) $ zip ["> 1️⃣ ", "> 2️⃣ ", "> 3️⃣ ", "> 4️⃣ ", "> 5️⃣ "] $ options poll) 

helpIntent :: IntentResolver
helpIntent = intent help'

help' :: BotOp
help' conf ev@(MessageCreate c user m) _ = void $ runReq defaultHttpConfig $ sendSimpleMessage conf c $
  "Command **poll**:\n"
  <> "Creates a poll that can be interacted by using reactions. The message will be updated when votes are added or removed "
  <> "to show the current winning options.\n\n"
  <> "**Usage**:\n"
  <> "> >> poll _TITLE_\n"
  <> "> _OPTION 1_\n"
  <> "> _OPTION 2_\n"
  <> "> ...\n"
  <> "At the moment, **poll supports only up to 5 options**. This limit is imposed to optimize the speed in creating and counting polls, "
  <> "as some bot actions (such as creating emojis on poll start) are rate-limited by the Discord API."