{-# LANGUAGE OverloadedStrings #-}

module Bot.Content.Roll (
  rollIntent,
  helpIntent,
  prefix
) where
  import qualified Text.Parsec as Parsec
  import Text.Parsec.Text (Parser)
  import qualified Data.Text as T
  import Control.Monad
  import Data.Maybe
  import System.Random

  import Network.HTTP.Req
  import Bot.Intent (intent, BotOp, IntentResolver)
  import Discord.Comms

  prefix :: T.Text
  prefix = "roll"

  rollIntent :: IntentResolver
  rollIntent = intent doRolls

  helpIntent :: IntentResolver
  helpIntent = intent f
    where
      f :: BotOp
      f conf e@(MessageCreate ch _ _) _ = void . runReq defaultHttpConfig $ sendSimpleMessage conf ch $
        "Command `roll`: \n"
        <> "Rolls a set of the specified dice listed in `XdN` notation.\n\n"
        <> "Example usage:\n"
        <> "> >> roll 1d20 2d4 3d6\n"

  data DiceSet = Roll Int Int
  type DiceSetup = [ DiceSet ]

  instance Show DiceSet where
    show (Roll n i) = show n <> "d" <> show i

  pDiceRolls :: Parser DiceSetup
  pDiceRolls = Parsec.many1 (do {
      n <- (read :: String -> Int) <$> Parsec.many1 (Parsec.oneOf "0123456789")
    ; Parsec.char 'd'
    ; x <- (read :: String -> Int) <$> Parsec.many1 (Parsec.oneOf "0123456789")
    ; Parsec.spaces
    ; pure $ Roll n x
    });

  parseDice :: T.Text -> Either Parsec.ParseError DiceSetup
  parseDice = Parsec.parse pDiceRolls ""

  rollOne :: DiceSet -> [IO (Int, Int)]
  rollOne (Roll 0 _) = []
  rollOne (Roll n x) = ((flip (,) x) <$> (genRand 1 x)) :  rollOne (Roll (n-1) x)

  rollAll :: DiceSetup -> [IO (Int, Int)]
  rollAll (x:xs) = rollOne x <> rollAll xs
  rollAll [] = []
  
  genRand :: Int -> Int -> IO Int
  genRand a b = fst . randomR (a, b) <$> newStdGen

  doRolls :: BotOp
  doRolls conf e@(MessageCreate ch u msg) conn = case parseDice (fromMaybe "" $ T.strip <$> (T.stripPrefix prefix $ messageContent msg)) of
        Left _        -> mempty
        Right diceSet -> (prettyPrintRolls <$> (sequence . rollAll $ diceSet)) 
          >>= void . runReq defaultHttpConfig . sendSimpleMessage conf ch
    where
      prettyPrintRolls :: [(Int, Int)] -> T.Text
      prettyPrintRolls ((s, n):rolls) = "**d" <> (T.pack $ show n) <> ":** " <> (T.pack $ show s) <> "\n"
        <> prettyPrintRolls rolls
      prettyPrintRolls [] = ""
  doRolls _ _ _ = mempty
