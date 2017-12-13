module Day4
  ( p1
  , p2
  , run
  )
  where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Digest.Pure.MD5 (md5)
import Data.Text (strip, pack, unpack)

run :: IO ()
run = do
  input <- trim <$> readFile "input/day4"
  print $ p1 input
  print $ p2 input
  where
    trim :: String -> String
    trim =
      unpack . strip . pack

p1 :: String -> Int
p1 input =
  head [x | x <- [1..], md5Take 5 (input ++ show x) == "00000"]

p2 :: String -> Int
p2 input =
  head [x | x <- [1..], md5Take 6 (input ++ show x) == "000000"]

md5Take :: Int -> String -> String
md5Take x =
  take x . show . md5 . Char8.pack

