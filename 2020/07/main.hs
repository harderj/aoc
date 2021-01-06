
import Data.Function
import qualified Data.Set as Set

main :: IO ()
main = do
  cts <- readFile "input.dat"
  putStrLn $ "part 1: " ++ (show $ part1 cts)
  putStrLn $ "part 2: " ++ (show $ part2 cts)

part1 :: String -> Int
part1 cts = let s = loadSpecification cts
  in length $ ancestors s "shiny gold"

part2 :: String -> Int
part2 cts = undefined

data Edge k a = E k a
  deriving (Show, Eq, Ord)
data Node k a = N k (Set.Set (Edge k a))
  deriving (Show, Eq, Ord)
type Graph k a = Set.Set (Node k a)

getKey :: Node k a -> k
getKey (N k _) = k

type Bag = Node String Int
type Specification = Set.Set Bag

loadSpecification :: String -> Specification
loadSpecification cts = let
  lns :: [String]
  lns = lines cts
  in Set.fromList $ map readBag lns

readBag :: String -> Bag
readBag ln = let
  (a1:c1:_:_:wds1) = words ln
  key = unwords [a1, c1]
  h :: [String] -> [Edge String Int]
  h (n2:a2:c2:_:wds2) = E (unwords [a2, c2]) (read n2) : h wds2
  h _ = []
  in N key (Set.fromList $ h wds1)

isChildOf :: (Eq k) => k -> Node k a -> Bool
isChildOf k (N _ es) = any (\(E k' _) -> k == k') es

parents :: (Eq k) => Graph k a -> k -> Graph k a
parents g k = Set.filter (isChildOf k) g

oldies :: (Ord k, Ord a) => Graph k a -> Graph k a -> Graph k a
oldies g ps = let qs = g Set.\\ ps
  in Set.union ps $ Set.unions $ Set.map (\(N k' _) -> parents qs k') ps

ancestors :: (Ord k, Ord a) => Graph k a -> k -> Graph k a
ancestors g k = fixEq (oldies g) $ parents g k

fixEq :: (Eq a) => (a -> a) -> a -> a
fixEq f x = let y = f x in
  if x == y then x else fixEq f y


