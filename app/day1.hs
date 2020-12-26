module Main where

dup = map (\x -> (x, x))

addmult :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addmult (x, y) (w, z) = (x+w, y*z) 



rotate :: Int -> [Int] -> [Int]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

calc :: Int -> [Int] -> [(Int,Int)]
calc _ [] = []
calc n xs = zipWith addmult (dup (rotate n xs)) (dup xs)

findValue :: Int -> [Int] -> Int
findValue n inp =
    case ans of
        [] -> 0::Int
        x:_ -> snd x
      where 
       combined = calc n inp
       ans = filter (\(x,_) -> x == 2020) combined
        
    
main :: IO ()
main = do
    val <- getContents
    let d = lines val
    let dd = map (read::String->Int) d
    let len = length dd
    let results = map (`findValue` dd) [1 .. len]
    let val = filter (/= 0) results
    print val

 
