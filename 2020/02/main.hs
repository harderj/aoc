
import Foreign.Marshal.Utils
import Data.List.Split

readInput = readFile "input.dat"

-- list2Tuple lst = read $ "(" ++ (init.tail.show) lst ++ ")"

main :: Num x => IO x
main = do
  str <- readFile "input.dat"
  return $ processInput str

-- processInput :: Num x => String -> x
processInput input = let
  lines_ = map words $ lines input 
  take2 :: [String] -> (Int, Int) 
  take2 (a:b:_) = (read a, read b)
  take2 a = error $ "Input error in range: " ++ show a
  fromWords (w1:w2:w3:_) =
    (take2 $ splitOn ['-'] w1
    ,head w2
    ,w3)
  validate ((a, b), c, w) =
    between a b $ countElem c w
  in sum $ map (fromBool . validate . fromWords) lines_

-- countElem :: (Eq e, Num x) => e -> [e] -> x
countElem y (x:xs) = fromBool (x == y) + countElem y xs
countElem _ [] = 0

-- between :: Ord a => a -> a -> a -> Bool
between a b c = (a <= c && c <= b)




