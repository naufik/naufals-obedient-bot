{-# LANGUAGE OverloadedStrings #-}

module Discord.Config ( 
  DiscordConfig ( .. ),
  loadDiscordConfig
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Aeson
import System.Environment

data DiscordConfig = DiscordConfig {
  clientId :: B.ByteString,
  clientSecret :: B.ByteString,
  botAccessToken :: B.ByteString
}

-- TODO: replace with lookupEnv!
loadDiscordConfig :: IO DiscordConfig
loadDiscordConfig =
  mapM getEnv ["DISCORD_CLIENT_ID", "DISCORD_CLIENT_SECRET", "DISCORD_BOT_ACCESS_TOKEN"] >>=
  \[a0, a1, a2] -> pure $ DiscordConfig { clientId = BU.fromString a0,
    clientSecret = BU.fromString a1,
    botAccessToken = BU.fromString a2 }