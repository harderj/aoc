import Foreign.Marshal.Utils
import qualified Data.Set as Set

main = do
  cts <- readFile "input.dat"
  print $ sum $ map fromBool $ process cts

-- process :: String -> Int
process cts = let
  lns :: [String]
  lns = lines cts
  blocks :: [String]
  blocks = map unwords $ splitOn "" lns
  -- help :: String -> [String]
  help b = let 
    ws :: [String]
    ws = words b
    pairs :: [[String]]
    pairs = map (splitOn ':') ws
    heads :: [String]
    heads = map head pairs
    s :: Set.Set String
    s = Set.fromList heads
    relevant :: [String]
    relevant = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    hEq :: Set.Set String -> Bool
    hEq x = (x Set.\\ (Set.fromList ["cid"])) == Set.fromList relevant
    allThere :: Bool
    allThere = hEq s
    in allThere && all help2 pairs
  in map help blocks

help2 :: [String] -> Bool
help2 ("byr":code:_) = between 1920 2002 $ read code
help2 ("iyr":code:_) = between 2010 2020 $ read code
help2 ("eyr":code:_) = between 2020 2030 $ read code
help2 ("hgt":code:_) = checkHeight code
help2 ("hcl":code:_) = checkHcl code
help2 ("ecl":code:_) = Set.member code $ Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
help2 ("pid":code:_) = (length code == 9) && all isNumber code
help2 ("cid":_) = True

checkHcl :: String -> Bool
checkHcl ('#':xs) = (all h xs) && (length xs == 6)
  where h x = (between '0' '9' x) || (between 'a' 'f' x)
checkHcl _ = False

checkHeight :: String -> Bool
--checkHeight str = takeWhile isNumber 
checkHeight (x:y:"in") = between 59 76 $ (read [x,y])
checkHeight (x:y:z:"cm") = between 150 193 $ (read [x,y,z])
checkHeight _ = False


isNumber :: Char -> Bool
isNumber '0' = True
isNumber '1' = True
isNumber '2' = True
isNumber '3' = True
isNumber '4' = True
isNumber '5' = True
isNumber '6' = True
isNumber '7' = True
isNumber '8' = True
isNumber '9' = True
isNumber _ = False

between :: (Ord x) => x -> x -> x -> Bool
between x y z = x <= z && z <= y

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn y ys = help [] ys
  where help zs (x:xs) =
          if x == y
          then zs : help [] xs
          else help (zs ++ [x]) xs
        help zs [] = [zs]

