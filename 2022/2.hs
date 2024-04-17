import Common

main = do
  contents <- readFile "inputs/2.txt"
  let
    part1 = sum $ map evalRound $ lines contents
    evalRound i = case i of
      [opp, ' ', you] -> matchPoints opp you + optionPoints you
      e -> error ("not valid input for round: " ++ show e)
    optionPoints you = case you of
      'X' -> 1
      'Y' -> 2
      'Z' -> 3
    matchPoints opp you = case (opp, you) of
      ('A','X') -> 3
      ('B','Y') -> 3
      ('C','Z') -> 3
      ('A','Y') -> 6
      ('B','Z') -> 6
      ('C','X') -> 6
      ('A','Z') -> 0
      ('B','X') -> 0
      ('C','Y') -> 0
      _ -> error ("not valid input for match: " ++ [opp, you])
  print (part1, "part 2 not done yet")
