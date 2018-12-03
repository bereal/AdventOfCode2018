import Data.Set (Set, empty, member, insert)
import Common

parseInt :: String -> Int
parseInt = read . dropWhile (== '+')

solve 1 input = sum input
solve 2 input =
    let sums = scanl (+) 0 (cycle input)
    in findRepeated sums

touch :: Ord a => a -> Set a -> (Bool, Set a)
touch a m = let seen = member a m in (seen, insert a m)

findRepeated :: Ord a => [a] -> a
findRepeated xs = find empty $ xs
    where find m (x:xs) = let (seen, m') = touch x m
                          in if seen then x else find m' xs

main = do
    lines <- readInput 1
    let input = map parseInt lines
    print $ solve 1 input
    print $ solve 2 input