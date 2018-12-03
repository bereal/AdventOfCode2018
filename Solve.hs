{-# LANGUAGE NamedFieldPuns #-}

import System.Environment
import qualified Data.Set as Set

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

main = do
    args <- getArgs
    let day = read $ head args
    let filename = concat ["input_", show day, ".txt"]
    input <- readFile filename >>= return . lines
    putStrLn $ solve $ Problem day 1 input
    putStrLn $ solve $ Problem day 2 input

