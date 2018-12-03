{-# LANGUAGE NamedFieldPuns #-}

import System.Environment
import qualified Data.Set as Set
import qualified Data.Map as Map

data Problem = Problem { day :: Int, part :: Int, input :: [String] }

solve :: Problem -> String

-- Problem 1 ----------------------------------------------

parseInt :: String -> Int
parseInt = read . dropWhile (== '+')

solve Problem { day = 1, part = 1, input } = show $ sum $ map parseInt input

solve Problem { day = 1, part = 2, input } =
    show $ solve' Set.empty 0 (cycle $ map parseInt input)
        where solve' s freq (x:xs) = let freq' = freq + x
                                     in if Set.member freq' s
                                        then freq'
                                        else solve' (Set.insert freq' s) freq' xs

-- Problem 2 ----------------------------------------------

solve Problem { day = 2, part = 1, input } =
    let (twos, threes) = unzip $ map countTwosAndThrees input
        countTrues = length . filter id
    in show $ (countTrues twos) * (countTrues threes)


solve Problem { day = 2, part = 2, input } =
    let words = map (filter (/= ' ')) input
    in uncurry common $ head $ filter (uncurry areSimilar) $ pairs words

countTwosAndThrees :: Ord a => [a] -> (Bool, Bool)
countTwosAndThrees s = let counter = Map.elems $ countVals s
                     in (elem 2 counter, elem 3 counter)

type Counter a = Map.Map a Int

increment :: Ord a => a -> Counter a -> Counter a
increment a c = Map.insert a (1 + (Map.findWithDefault 0 a c)) c

countVals :: Ord a => [a] -> Counter a
countVals = foldl (flip increment) Map.empty

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
    args <- getArgs
    let day = read $ head args
    let filename = concat ["input_", show day, ".txt"]
    input <- readFile filename >>= return . lines
    putStrLn $ "part 1: " ++ (solve $ Problem day 1 input)
    putStrLn $ "part 2: " ++ (solve $ Problem day 2 input)

