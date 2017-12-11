module Day2
  ( p1
  , p2
  , run
  )
  where

import Data.Char
import Data.Maybe
import Data.List
import Text.ParserCombinators.ReadP

type Length = Int
type Width = Int
type Height = Int
type Dimension = (Length, Width, Height)

run :: IO ()
run = do
  input <- readFile "input/day2"
  print $ p1 input
  print $ p2 input

p1 :: String -> Int
p1 input =
  sum $ map compute (dimensions input)
  where
    compute :: Dimension -> Int
    compute (l, w, h) =
      let dims = [l*w, w*h, h*l]
      in sum (map (2*) dims) + minimum dims

p2 :: String -> Int
p2 input = 
  sum $ map compute (dimensions input)
  where
    compute :: Dimension -> Int
    compute (l, w, h) =
      let dims = [l, w, h]
          halfPerim = sum . take 2 $ sort dims
          volume = product dims
      in 2 * halfPerim + volume
        
dimensions :: String -> [Dimension]
dimensions input =
  map (fromJust . parseDimension) $ lines input

parseDimension :: String -> Maybe Dimension
parseDimension input =
  case readP_to_S parser input of
    [] -> Nothing
    ((result,_):_) -> Just result
  where
    parser :: ReadP Dimension
    parser = do
      l <- digit; x
      w <- digit; x
      h <- digit; eof
      return (l, w, h)
      where
        digit :: ReadP Int
        digit = fmap read (many1 $ satisfy isDigit)
        x :: ReadP Char
        x = satisfy (== 'x')

