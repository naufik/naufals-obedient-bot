{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Maybe
import Data.Foldable (sequence_)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types
import Discord.Comms
import Discord.Config

import Bot.Intent
import Bot.Content

import Wuss
import Network.HTTP.Req
import Network.WebSockets
import System.Environment

main :: IO ()
main = withDefault runApp

runApp :: DiscordConfig -> IO ()
runApp x =
    putStrLn "begin" >>
    runSecureClient "gateway.discord.gg" 443 "/"
    (createGatewayListener x (createListenerFromIntents botIntents))