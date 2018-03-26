{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.Set
import System.Environment
import System.IO

birthdayCakeCandles :: Int -> [Int] -> Int
birthdayCakeCandles n ar = countMaxHeights 0 0 ar
  
countMaxHeights n h [] = n
countMaxHeights n h (x:xs)
  | x < h = countMaxHeights n h xs
  | x == h = countMaxHeights (n+1) h xs
  | otherwise = countMaxHeights 1 x xs

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    n <- readLn :: IO Int
    arTemp <- getLine
    let ar = Data.List.map (read :: String -> Int) . words $ arTemp
    let result = birthdayCakeCandles n ar
    putStrLn $ show result

