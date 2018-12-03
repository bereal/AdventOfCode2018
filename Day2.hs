import qualified Data.Map as Map
import Common

solve 1 input =
    let (twos, threes) = unzip $ map countTwosAndThrees input
        countTrues = length . filter id
    in show $ (countTrues twos) * (countTrues threes)

solve 2 input =
    uncurry common $ head $ filter (uncurry areSimilar) $ pairs input


countTwosAndThrees :: Ord a => [a] -> (Bool, Bool)
countTwosAndThrees s = let counter = Map.elems $ countVals s
                     in (elem 2 counter, elem 3 counter)


atMost (-1) _ = False
atMost _ [] = True
atMost v (x:xs) = let v' = if x then v - 1 else v in atMost v' xs

areSimilar a b = atMost 1 $ map (uncurry (/=)) $ zip a b

-- find common elements in the lists
common a b = map fst $ filter (uncurry (==)) $ zip a b

-- all possible pairs (upto the order)
pairs [] = []
pairs (x:xs) = (map (\y -> (x, y)) xs) ++ pairs xs


main = do
    input <- readInput 2
    putStrLn $ solve 1 input
    putStrLn $ solve 2 input