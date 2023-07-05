module Main where

import Lib 

wordList :: [String]
wordList = ["apple", "banana", "orange", "grape", "kiwi"]

main :: IO ()
main = do
  randomWord <- chooseRandomWord wordList
  putStrLn $ "Randomly chosen word: " ++ randomWord



