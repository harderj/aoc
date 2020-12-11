main = do
  str <- readFile "input.dat"
  print $ processInput str

processInput :: String -> Int
processInput input = let
  numbers :: [Int]
  numbers = map read $ lines input 
  pairs = [(i, j) | i <- numbers, j <- numbers, i + j == 2020]
  in uncurry (*) $ head pairs


