import Data.List.Split

checkHeight xs = case (value, unit) of
    (v, "m") -> (v >= 150) && (v <= 193)
    (v, "n") -> (v >= 59) && (v <= 76)
    _         -> False
  where
      d = splitOneOf "ci" xs 
      value = ((read::String->Int) (head d))
      unit = if (length d) == 2 
                then head (tail d)
                else ""

checkHcl :: String -> Bool
checkHcl xs = case xs of
    '#':yx -> ((length yx) == 6) && (all (==True) $ map (\x -> elem x ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd','e', 'f']) yx)
    _      -> False

check :: String -> Bool
check x = case id of
   "byr":xs -> ((((read::String->Int) (head xs))) > 1919) && ((((read::String->Int) (head xs))) < 2003)
   "iyr":xs -> ((((read::String->Int) (head xs))) > 2009) && ((((read::String->Int) (head xs))) < 2021)
   "eyr":xs -> ((((read::String->Int) (head xs))) > 2019) && ((((read::String->Int) (head xs))) < 2031)
   "hgt":xs -> checkHeight (head xs)
   "hcl":xs -> checkHcl (head xs)
   "ecl":xs -> elem (head xs) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
   "pid":xs -> ((length (head xs)) == 9) && (all (==True) (map (\x -> elem x ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) (head xs)))
   _        -> True
  where id = splitOn ":" x 


main :: IO ()
main = do
    passports <- getContents
    let d = splitOn "\n\n" passports
    let dd = map (map (\x->if x == '\n' then ' ' else x)) d
    let ddd = map (splitOn " ") dd
    let dddd = map (filter (\x -> (take 3 x) /= "cid")) ddd
    let e = filter (\x -> (length x) >= 7) dddd
    let f = map (map check) e
    let g = map (all (==True)) f
    print (length $ filter (==True) g)
