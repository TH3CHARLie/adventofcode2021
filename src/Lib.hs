module Lib
    (
        day1Part1Solution,
        day1Part2Solution
    ) where

import System.Environment ( getArgs )


getNumbers :: String -> [Int]
getNumbers str = [read x :: Int | x <- words str]

countIncreasedMeasurement :: [Int] -> Int
countIncreasedMeasurement ls =
    length $ filter (< 0) [i - j | (i, j) <- zip ls (tail ls)]


countIncreasedMeasurementWindow :: [Int] -> Int
countIncreasedMeasurementWindow ls = x where
    b = tail ls
    c = tail b
    lsWindow = [i + j + k | (i, (j, k)) <- zip c $ zip ls b]
    x = countIncreasedMeasurement lsWindow


day1Part1Solution :: IO ()
day1Part1Solution = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let numbers = getNumbers contents
    let increase = countIncreasedMeasurement numbers
    print increase

day1Part2Solution :: IO ()
day1Part2Solution = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let numbers = getNumbers contents
    let increase = countIncreasedMeasurementWindow numbers
    print increase