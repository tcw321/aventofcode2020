module Main where

import Data.List.Split

processLine x = 
    c == 1
    where
        splitString = splitOneOf " :" x
        minMax = splitOn "-" $ head splitString
        min = (read::String->Int) (head minMax)
        max = (read::String->Int) (head (tail minMax))
        alpha = head (head (tail splitString))
        passwd = (head (tail (tail (tail splitString))))
        r = [passwd!!(min-1), passwd!!(max-1)]
        c = length $ filter (== alpha) r 

main :: IO ()
main = do
    passwords <- getContents
    let d = lines passwords
    let results = map processLine d
    let count = length $ filter (== True) results
    print count
