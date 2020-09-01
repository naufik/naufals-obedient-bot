
{-# LANGUAGE OverloadedStrings #-}
module Bot.Content.PythonRunner (
  prefix,
  runPythonIntent,
  helpIntent
) where

import qualified Data.Text as T
import Bot.Intent
import System.Process
import Control.Monad
import GHC.IO.Handle
import Control.Concurrent
import Network.HTTP.Req
import Data.Maybe
import Control.Applicative ((<|>))
import qualified Text.Parsec as Parsec

import Discord.Config
import Discord.Comms

-- WhiteList
whitelistedChannels :: [ T.Text ]
whitelistedChannels = []

prefix :: T.Text
prefix = "py"

runPythonIntent :: IntentResolver
runPythonIntent = intent $ \conf e@(MessageCreate _ u m) conn -> case commandParser (stripMessage m) of
  Left pe -> (putStrLn . show $ stripMessage m) >> (putStrLn $ show pe)
  Right t -> (putStrLn $ show t) >> runPython' conf e t
  where
    stripMessage m = fromMaybe "" $ (T.stripPrefix "py") (messageContent m) >>= return . T.strip

commandParser :: T.Text -> Either Parsec.ParseError T.Text
commandParser = Parsec.parse (Parsec.between ((void $ Parsec.string "```") <> Parsec.optional (Parsec.string "py" <> Parsec.option "" (Parsec.string "thon")) <> (void $ Parsec.string "\n")) (Parsec.string "```") ((Parsec.manyTill Parsec.anyToken (Parsec.try . Parsec.lookAhead $ Parsec.string "```")) >>= return . T.pack)) ""

-- YES, I am aware of how not secure this shit is. This is an experiement so far and I wrote this in like, what, 2 hors.
runPython' :: DiscordConfig -> DiscordEvent -> T.Text -> IO ()
runPython' conf e@(MessageCreate c _ _) script = do {
    (_, hOut', _, hProc) <- createProcess $ (shell . T.unpack $ "echo \"" <> escape script <> "\" | python"){ std_out = CreatePipe }
  ; case hOut' of 
    Nothing -> mempty
    Just hOut -> void . forkIO $ consumeRecursive hOut ""
  }
  where
    sendContent :: String -> IO ()
    sendContent strStdOut = let stdOut = T.pack strStdOut in
      void . runReq defaultHttpConfig $ sendSimpleMessage conf c $ 
        "Finished. Stdout: ```\n"
        <> stdOut
        <> "\n```"
    
    escape :: T.Text -> T.Text
    escape = T.replace "\"" "\\\""

    consumeRecursive :: Handle -> String -> IO ()
    consumeRecursive hOut s = do {
      toStop <- (hIsOpen hOut >>= pure . not)
    ; if toStop then sendContent s else (hGetContents hOut >>= consumeRecursive hOut . (s++))
    }

helpIntent :: IntentResolver
helpIntent = intent helpIntent'

helpIntent' :: BotOp
helpIntent' conf e@(MessageCreate c _ _) _ = void . runReq defaultHttpConfig $ sendSimpleMessage conf c $
  "Command **py**:\n"
  <> "Executes a Python script inside the given code block, the code block **may** be marked as `py` or `python`\n\n"
  <> "Example"
  <> "> >> py ```python\n"
  <> "> print('Hello World')\n"
  <> "> ```\n\n"
  <> "This command has a lot of limitations, including:\n"
  <> "- No ability to read from `stdin` (or `input()`).\n"
  <> "- Only includes stuff from the python standard library.\n"
  <> "- Whitelisted to some servers only due to possible code injection.\n"