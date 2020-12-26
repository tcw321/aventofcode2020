import Data.List
import Data.List.Split
import Data.Sort

main :: IO ()
main = do
    questions <- getContents
    let d = splitOn "\n\n" questions
    let dd = map (filter (/='\n')) d
    let ddd = map (map head . group . sort) dd
    let results = foldr (+) 0 $ map length ddd
    print results
