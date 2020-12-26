module Main where

countTrees trees rt dwn = 
    length $ filter (== '#') tree
    where
        d = if dwn > 1 then map snd ( filter (\(x,_) -> odd x) (zip [1..] trees))
                       else trees
        dd = drop 1 d
        offsets = map (*rt) [1..]
        treesWithOffsets = zip offsets dd
        align = map (\(a,b) -> drop a b) treesWithOffsets
    --let r = map (take 10) align
    --print r
        tree = map head align

main :: IO ()
main = do
    trees <- getContents
    let d = lines trees
    let dd = map cycle d
    let count1 = countTrees dd 1 1
    let count2 = countTrees dd 3 1
    let count3 = countTrees dd 5 1
    let count4 = countTrees dd 7 1
    let count5 = countTrees dd 1 2
    --let ddd = drop 1 dd
    --let offsets = map (*3) [1..]
    --let treesWithOffsets = zip offsets ddd
    --let align = map (\(a,b) -> drop a b) treesWithOffsets
    --let r = map (take 10) align
    --print r
    --let tree = map head align
    --let count = length $ filter (== '#') tree
    let count = count1*count2*count3*count4*count5
    print count
