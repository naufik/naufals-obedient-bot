module Main where

import Discord.Comms
import Discord.Config


import Wuss
import Network.WebSockets
import System.Environment

main :: IO ()
main = withDefault runApp

runApp :: DiscordConfig -> IO ()
runApp x = runSecureClient "gateway.discord.gg" 443 "/"
    $ createGatewayListener x (\config event conn -> putStrLn . show $ event)