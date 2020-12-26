
findSeat code (low, high) = 
  case code of
    'L':[]   -> low
    'R':[]   -> high  
    'L':rest -> findSeat rest (low, low + (quot (high-low) 2))
    'R':rest -> findSeat rest (low + (quot (high-low) 2) + 1, high)
    _  -> 0


findRow code (low, high) = 
  case code of
    'F':[]   -> low
    'B':[]   -> high  
    'F':rest -> findRow rest (low, low + (quot (high-low) 2))
    'B':rest -> findRow rest (low + (quot (high-low) 2) + 1, high)
    _  -> 1000

main :: IO ()
main = do
    input <- getContents
    let seats = lines input
    let rows = map (take 7) seats
    let cols = map (drop 7) seats
    let rowNum = map (\x -> findRow x (0,127)) rows
    let seatNum = map (\x -> findSeat x (0, 7)) cols
    let comb = zip rowNum seatNum
    let numbers = map (\(x,y) -> x*8 + y) comb
    print $ maximum numbers  


