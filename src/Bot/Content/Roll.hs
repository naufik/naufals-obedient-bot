{-# LANGUAGE OverloadedStrings #-}

module Bot.Content.Roll (
  DiceSet,
  DiceSetup,
  merge1,
  merge,
  parseDice
) where
  import qualified Text.Parsec as Parsec
  import Text.Parsec.Text (Parser)
  import qualified Data.Text as T
  import System.Random

  data DiceSet = Roll Int Int
  type DiceSetup = [ DiceSet ]

  instance Show DiceSet where
    show (Roll n i) = show n <> "d" <> show i
  
  merge1 :: DiceSet -> DiceSetup -> DiceSetup
  merge1 d@(Roll n i) (d'@(Roll n' i'):xs) = if i' == i then (Roll (n+n') i):xs else (d':merge1 d xs)
  merge1 (Roll n i) [] = [ Roll n i ]

  merge :: Foldable t => t DiceSet -> DiceSetup
  merge m = foldr merge1 [] m

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
  rollOne (Roll n x) = [(flip (,) $ x) <$> (genRand 1 x)] ++ rollOne (Roll (n-1) x)

  rollAll :: DiceSetup -> [IO (Int, Int)]
  rollAll (x:xs) = rollOne x <> rollAll xs
  rollAll [] = []
  
  genRand :: Int -> Int -> IO Int
  genRand a b = fst . randomR (a, b) <$> newStdGen