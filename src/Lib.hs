module Lib
    (
        day1Part1Solution,
        day1Part2Solution,
        day2Part1Solution,
        day2Part2Solution
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

getCommand :: [String] -> [(String, Int)]
getCommand ls =
    let tokenized = map words ls in
        [(head x, read (last x) :: Int) | x <- tokenized]

getPositionOffset :: (String, Int) -> Int
getPositionOffset ("forward", x)= x
getPositionOffset _ = 0

getDepthOffset :: (String, Int) -> Int
getDepthOffset ("down", x) = x
getDepthOffset ("up", x)   = -x
getDepthOffset _   = 0


getPosition :: [(String, Int)] -> Int
getPosition ls = sum $ map getPositionOffset ls

getDepth :: [(String, Int)] -> Int
getDepth ls = sum $ map getDepthOffset ls

day2Part1Solution :: IO ()
day2Part1Solution = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let ls = lines contents
    let commands = getCommand ls
    let pos = getPosition commands
    let depth = getDepth commands
    print (pos * depth)

getDepthWithAim :: [(String, Int)] -> Int
getDepthWithAim ls = res where
    aimChanges = map getPositionOffset ls
    depthOffsets = map getDepthOffset ls
    aim = scanl1 (+) depthOffsets
    res = sum [i * j | (i, j) <- zip aimChanges aim]

day2Part2Solution :: IO ()
day2Part2Solution = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let ls = lines contents
    let commands = getCommand ls
    let pos = getPosition commands
    let depth = getDepthWithAim commands
    print (pos * depth)
