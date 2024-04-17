import qualified System.IO as IO
import qualified Data.List.Split

import Common

main = do
  contents <- IO.readFile "inputs/1.txt"
  let
    f = lines >.> Data.List.Split.splitOn [""] 
      >.> filter (/= [""]) >.> map (map read) >.> map sum
      >.> maximum
    res = f contents 
  print res
