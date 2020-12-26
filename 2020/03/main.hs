import Foreign.Marshal.Utils

main = do
  cts <- readFile "input.dat"
  print $ process cts


process inp = let
  lns = lines inp
  w = length $ head lns
  idxs = [3 * i `mod` w | i <- [0..]]
  encs = zipWith (!!) lns idxs
  in countElem '#' encs


--countElem :: (Eq e, Num x) => e -> [e] -> x
countElem y (x:xs) = fromBool (x == y) + countElem y xs
countElem _ [] = 0



