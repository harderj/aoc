
import qualified Data.Set as Set

main :: IO ()
main = do
  cts <- readFile "input.dat"
  print $ process cts

data Edge k a = E k a
  deriving (Show, Eq, Ord)
data Node k a = N k (Set.Set (Edge k a))
  deriving (Show, Eq, Ord)
type Graph k a = Set.Set (Node k a)

type Bag = Node String Int
type Specification = Set.Set Bag

process :: String -> Int
process cts = let
  s = loadSpecification cts
  in countParents s "shiny gold"

loadSpecification :: String -> Specification
loadSpecification cts = let
  lns :: [String]
  lns = lines cts
  in Set.fromList $ map readBag lns

readBag :: String -> Bag
readBag ln = let
  (a1:c1:_:_:wds1) = words ln
  key = a1 ++ c1
  h :: [String] -> [Edge String Int]
  h (n2:a2:c2:_:wds2) = E (a2 ++ c2) (read n2) : h wds2
  h _ = []
  in N key (Set.fromList $ h wds1)
  
countParents :: Specification -> String -> Int
countParents s 


