import Data.List
import Data.List.Split ( splitOn )
import Data.Maybe (isJust)

type Rule = (Int, Int)
type Update = [Int]

parseInput :: String -> ([Rule], [Update])
parseInput input =
    let (ruleLines, updateLines) = break null (lines input)
        rules = map parseRule ruleLines
        updates = map parseUpdate (tail updateLines)
    in (rules, updates)

parseRule :: String -> Rule
parseRule line =
    let [xStr, yStr] = splitOn "|" line
        x = read xStr
        y = read yStr
    in (x, y)

parseUpdate :: String -> Update
parseUpdate line = map read $ splitOn "," line

isValidUpdate :: [Rule] -> Update -> Bool
isValidUpdate rules update = all (checkRule update) rules

checkRule :: Update -> Rule -> Bool
checkRule update (x,y) =
    case (elemIndex x update, elemIndex y update) of
        (Just ix, Just iy) -> ix < iy -- x must come before y
        _ -> True -- If either x or y is not present, the rule is satisfied

middlePage :: Update -> Int
middlePage update = update !! (length update `div` 2)

processUpdates :: [Rule] -> [Update] -> Int
processUpdates rules updates = sum $ map middlePage $ filter (isValidUpdate rules) updates

topologicalSort :: [Rule] -> [Int] -> [Int]
topologicalSort rules = sortNodes []
    where
        sortNodes sorted [] = sorted
        sortNodes sorted unsorted =
            let candidates = [n | n <- unsorted, canBePlaced n unsorted]
                canBePlaced n unsortedNodes = all (\(x, y) -> x /= n || y `notElem` unsortedNodes) rules
            in case candidates of
                [] -> error "Cycle detected in rules"
                (n:_) -> sortNodes (sorted ++ [n]) (delete n unsorted)

reorderUpdate :: [Rule] -> Update -> Update
reorderUpdate = topologicalSort

processIncorrectUpdates :: [Rule] -> [Update] -> Int
processIncorrectUpdates rules updates =
    let incorrectUpdates = filter (not . isValidUpdate rules) updates
        reorderedUpdates = map (reorderUpdate rules) incorrectUpdates
    in sum $ map middlePage reorderedUpdates

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (rules, updates) = parseInput content

    print $ "Part 1: " ++ show (processUpdates rules updates)
    print $ "Part 2: " ++ show (processIncorrectUpdates rules updates)