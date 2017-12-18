module Day6
  ( p1
  , p2
  , run
  )
  where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP

type BoolGrid = V.Vector (V.Vector Bool)
type IntGrid = V.Vector (V.Vector Int)
type Point = (Int, Int)
data Command = Toggle | On | Off deriving (Show)
data Instruction = Ins Command Point Point deriving (Show)

run :: IO ()
run = do
  input <- readFile "input/day6"
  print $ p1 input
  print $ p2 input

p1 :: String -> Int
p1 input =
  countOn $ foldl runInstruction start $ map (fromJust . parseInstruction) $ lines input
  where
    start :: BoolGrid
    start =
      V.fromList $ map V.fromList [ [False | _ <- [1..1000]] | _ <- [1..1000]]
    countOn :: BoolGrid -> Int
    countOn grid =
      V.sum $ V.map (V.foldl (\acc v -> if v then acc + 1 else acc) 0) grid
    runInstruction :: BoolGrid -> Instruction -> BoolGrid
    runInstruction grid (Ins cmd (x1, y1) (x2, y2)) =
      V.imap (\i vy -> if i >= y1 && i <= y2
                        then update (action cmd) (x1, x2) vy
                        else vy) grid
    update :: (Bool -> Bool) -> (Int, Int) -> V.Vector Bool -> V.Vector Bool
    update fn (x1, x2) =
      V.imap (\i vx -> if i >= x1 && i <= x2
                        then fn vx
                        else vx)
    action :: Command -> (Bool -> Bool)
    action cmd =
      case cmd of On     -> (True ||)
                  Off    -> (False &&)
                  Toggle -> not

p2 :: String -> Int
p2 input = 
  countOn $ foldl runInstruction start $ map (fromJust . parseInstruction) $ lines input
  where
    start :: IntGrid
    start =
      V.fromList $ map V.fromList [ [0 | _ <- [1..1000]] | _ <- [1..1000]]
    countOn :: IntGrid -> Int
    countOn grid =
      V.sum $ V.map V.sum grid
    runInstruction :: IntGrid -> Instruction -> IntGrid
    runInstruction grid (Ins cmd (x1, y1) (x2, y2)) =
      V.imap (\i vy -> if i >= y1 && i <= y2
                        then update (action cmd) (x1, x2) vy
                        else vy) grid
    update :: (Int -> Int) -> (Int, Int) -> V.Vector Int -> V.Vector Int
    update fn (x1, x2) =
      V.imap (\i vx -> if i >= x1 && i <= x2
                        then fn vx
                        else vx)
    action :: Command -> (Int -> Int)
    action cmd =
      case cmd of On     -> (+1)
                  Off    -> \x -> if x > 0 then x - 1 else 0
                  Toggle -> (+2)


parseInstruction :: String -> Maybe Instruction
parseInstruction input =
  case readP_to_S parser input of
    [] -> Nothing 
    ((result,_):_) -> Just result
  where
    parser :: ReadP Instruction
    parser = do
      cmd <- command; skipSpaces
      p1 <- coord; string "through "
      p2 <- coord; eof
      return $ Ins cmd p1 p2
      where
        command :: ReadP Command
        command = do
          c <- string "turn on" <|> string "turn off" <|> string "toggle"
          return $ case c of "turn on" -> On
                             "turn off" -> Off
                             "toggle" -> Toggle
        coord :: ReadP Point
        coord = do
          x <- digit; char ','
          y <- digit; skipSpaces
          return (x, y)
        digit :: ReadP Int
        digit = fmap read (many1 $ satisfy isDigit)
