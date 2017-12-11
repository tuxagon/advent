module Day1
  ( p1
  , p2
  , run
  )
  where

run :: IO ()
run = do
  input <- readFile "input/day1"
  print $ p1 input
  print $ p2 input

p1 :: String -> Int
p1 input =
  getFloor $ foldl floorCount (0, 0) input
  where
    floorCount :: (Int, Int) -> Char -> (Int, Int)
    floorCount (op, cp) p =
      case p of
        '(' -> (op+1, cp)
        ')' -> (op, cp+1)
        _   -> (op, cp)
    getFloor (op, cp) =
      op - cp

p2 :: String -> Maybe Int
p2 input = 
  case findPosition (-1, 0, 0) input of
    0 -> Nothing
    p -> Just p
  where
    findPosition :: (Int, Int, Int) -> String -> Int
    findPosition (target, pos, floor) [] 
      | target == floor = pos
      | otherwise       = 0
    findPosition (target, pos, floor) (c:cs) 
      | target == floor = pos
      | otherwise       = 
        let newFloor = 
              case c of
                '(' -> floor + 1
                ')' -> floor - 1
                _   -> floor
        in findPosition (target, pos+1, newFloor) cs

