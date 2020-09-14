{-# LANGUAGE OverloadedStrings #-}
module Bot.Content.Metro (
  prefix,
  helpIntent
) where

import qualified Data.Text as T
import Bot.Intent
import Discord.Comms
import Network.HTTP.Req
import Control.Monad

prefix :: T.Text
prefix = "metro"

helpIntent :: IntentResolver
helpIntent = intent help'

help' :: BotOp
help' conf ev@(MessageCreate c user m) _ = void $ runReq defaultHttpConfig $ sendSimpleMessage conf c $
  "Command **metro**:\n"
  <> "Looks for departures from the specified Melbourne Metro station.\n\n"
  <> "**Usage**:\n"
  <> "> >> metro `STATION NAME`\n\n"
  <> "**Examples**:\n"
  <> "> >> metro croxton\n"
  <> "> >> metro melbourne central\n"
  <> "> >> metro flinders street\n"
  <> "> >> metro mc\n"
  <> "As shown above, some station names may have aliases.\n\n"
  <> "Metro Module (version 0.0.1)"