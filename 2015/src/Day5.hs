{-# OPTIONS -Woverlapping-patterns #-}

module Day5
  ( p1
  , p2
  , run
  )
  where

import Data.Maybe
import Text.Regex

data NaughtyNice = Naughty | Nice deriving (Show, Eq)

run :: IO ()
run = do
  input <- readFile "input/day5"
  print $ p1 input
  print $ p2 input

p1 :: String -> Int
p1 input =
  length . filter (== Nice) . map condition $ lines input
  where
    condition :: String -> NaughtyNice
    condition s =
      if (not . badWords) s && vowels s >= 3 && repeatLetter s
        then Nice
        else Naughty
    badWords :: String -> Bool
    badWords s =
      isJust $ matchRegex (mkRegex "(ab|cd|pq|xy)") s
    vowels :: String -> Int
    vowels =
      foldl (\acc x -> if x `elem` "aeiou" then acc + 1 else acc) 0 
    repeatLetter :: String -> Bool
    repeatLetter [] = False
    repeatLetter (c:cs) =
      snd $ foldl (\acc@(prev, ok) x -> if ok then acc else (x, x == prev)) (c, False) cs
    

p2 :: String -> Int
p2 input = 
  length . filter (== Nice) . map condition $ lines input
  where
    condition :: String -> NaughtyNice
    condition s =
      if letterPair s && repeatLetter s
        then Nice
        else Naughty
    letterPair :: String -> Bool
    letterPair [] = False
    letterPair s  =
      let front = take 2 s
          rest = drop 2 s
      in case matchRegex (mkRegex front) rest of
          Nothing -> letterPair $ drop 1 s
          Just _  -> True
    repeatLetter :: String -> Bool
    repeatLetter []    = False
    repeatLetter [_]   = False
    repeatLetter [_,_] = False
    repeatLetter s@(l:_:r:_) =
      if l == r
          then True
          else repeatLetter $ drop 1 s
