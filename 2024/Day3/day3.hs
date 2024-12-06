import Text.Regex.TDFA
import Data.Array ((!))

inputFile :: String
inputFile = "input.txt"

data Instruction = Mul Int Int
                 | Do
                 | Dont
                 deriving (Show)

parseMatch :: MatchText String -> Instruction
parseMatch mt =
  let g1 = fst (mt ! 1) -- Group 1: mul(x,y)
      g2 = fst (mt ! 2) -- Group 2: x
      g3 = fst (mt ! 3) -- Group 3: y
      g4 = fst (mt ! 4) -- Group 4: do()
      g5 = fst (mt ! 5) -- Group 5: don't()
    in case () of
      _ | not (null g1) ->
          let x = read g2
              y = read g3
          in Mul x y
        | not (null g4) -> Do
        | not (null g5) -> Dont
        | otherwise -> error "Unknown instruction"

processInstructions :: [Instruction] -> Int
processInstructions instrs = go instrs True 0
  where
    go [] _ total = total
    go (i:is) enabled total =
      case i of
        Do -> go is True total
        Dont -> go is False total
        Mul x y -> if enabled
                   then go is enabled (total + x * y)
                   else go is enabled total

part1 :: IO ()
part1 = do
  input <- readFile inputFile
  let pattern = "mul\\(([0-9]+),([0-9]+)\\)"
      matches :: [[String]]
      matches = input =~ pattern
      products = [read x * read y | (_:x:y:_) <- matches]
      total = sum products
  print $ "Part 1: " ++ show total

part2 :: IO ()
part2 = do
  input <- readFile inputFile
  let pattern = "(mul\\(([0-9]+),([0-9]+)\\))|(do\\(\\))|(don't\\(\\))"
      regex :: Regex
      regex = makeRegex pattern
      matches :: [MatchText String]
      matches = matchAllText regex input
      instrs = map parseMatch matches
      total = processInstructions instrs
  print $ "Part 2: " ++ show total