module Main where
  
main :: IO ()
main = do
    val <- getContents
    let d = lines val
    let dd = map (read::String->Int) d
    let len = length dd
    let addResults = (+) <$> dd <*> dd
    let addResults' = (+) <$> dd <*> addResults
    let multResults = (*) <$> dd <*> dd
    let multResults' = (*) <$> dd <*> multResults
    let combined = zip addResults' multResults'
    let results = filter (\(x,_) -> x == 2020) combined
    print results

 