module Day3
  ( p1
  , p2
  , run
  )
  where

import qualified Data.Map.Lazy as Map

type Coord = (Int, Int)
data Deliverer = Santa | RoboSanta

run :: IO ()
run = do
  input <- readFile "input/day3"
  print $ p1 input
  print $ p2 input

p1 :: String -> Int
p1 input =
  Map.size . fst $ foldl move (start (0, 0)) input
  where
    start :: Coord -> (Map.Map Coord Int, Coord)
    start c = 
      (Map.singleton c 1, c)

p2 :: String -> Int
p2 input = 
  Map.size . fst $ foldl move' (start (0, 0)) input
  where
    start :: Coord -> (Map.Map Coord Int, (Deliverer, Coord, Coord))
    start c =
      (Map.singleton c 1, (Santa, c, c))
    move' :: (Map.Map Coord Int, (Deliverer, Coord, Coord))
          -> Char
          -> (Map.Map Coord Int, (Deliverer, Coord, Coord))
    move' (houses, (del, sc, rsc)) dir =
      let (visited, newCoord) = move (houses, coord) dir
      in (visited, update newCoord)
      where
        coord =
          case del of Santa     -> sc
                      RoboSanta -> rsc
        update c =
          case del of Santa     -> (RoboSanta, c, rsc)
                      RoboSanta -> (Santa, sc, c)
      
move :: (Map.Map Coord Int, Coord) -> Char -> (Map.Map Coord Int, Coord)
move (houses, (x, y)) dir =
  (Map.insertWith (+) to 1 houses, to)
  where
    to :: Coord
    to = case dir of
          '^' -> (x, y+1)
          '>' -> (x+1, y)
          'v' -> (x, y-1)
          'V' -> (x, y-1)
          '<' -> (x-1, y)
          _   -> (x, y)

