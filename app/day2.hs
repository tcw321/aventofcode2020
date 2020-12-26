module Main where

import Data.List.Split

processLine x = 
    min <= number && number <= max
    where
        splitString = splitOneOf " :" x
        minMax = splitOn "-" $ head splitString
        min = (read::String->Int) (head minMax)
        max = (read::String->Int) (head (tail minMax))
        alpha = head (head (tail splitString))
        found = filter (== alpha) (head (tail (tail (tail splitString)))) 
        number = length found

main :: IO ()
main = do
    passwords <- getContents
    let d = lines passwords
    let results = map processLine d
    let count = length $ filter (== True) results
    print count
