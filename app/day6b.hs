import Data.List
import Data.List.Split
import Data.Sort

main :: IO ()
main = do
    questions <- getContents
    let d = splitOn "\n\n" questions
    let splitGroups = map (splitOn "\n") d
    let splitGroups' = map (filter (/="")) splitGroups
    let countInGroups = map length splitGroups'
    let dd = map (filter (/='\n')) d
    let ddd = map (group . sort) dd
    let combine = zip countInGroups ddd
    let results = map (\(x,y) -> (length $ (filter (\z -> (length z) == x) y))) combine
    print $ foldr (+) 0 results 


