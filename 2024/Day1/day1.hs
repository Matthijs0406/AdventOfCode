import System.IO
import Data.List (sort, group)

parseContent :: String -> [(Int, Int)]
parseContent content = map (\line -> let [c1,c2] = map read (words line) in (c1,c2)) (lines content)

part1 :: IO ()
part1 = do
    content <- readFile "input.txt"

    let pairs = parseContent content
        col1 = map fst pairs
        col2 = map snd pairs

    let sortedCol1 = sort col1
        sortedCol2 = sort col2

    let distances = zipWith (\x y -> abs (x - y)) sortedCol1 sortedCol2
        totalDiff = sum distances

    putStrLn $ "Total distance: " ++ show totalDiff

part2 :: IO ()
part2 = do
    content <- readFile "input.txt"

    let pairs = parseContent content
        col1 = map fst pairs
        col2 = map snd pairs

        counts = map (\g -> (head g, length g)) (group (sort col2))

        lookupCount n = maybe 0 id (lookup n counts)

        similarityScore = sum [x * lookupCount x | x <- col1]

    putStrLn $ "Similarity score: " ++ show similarityScore