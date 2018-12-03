import qualified Data.Set as Set
import Common

parseInt :: String -> Int
parseInt = read . dropWhile (== '+')

solve 1 input = sum input
solve 2 input =
    let sums = scanl (+) 0 (cycle input)
    in findRepeated sums

type Memory a = Set.Set a

touch :: Ord a => a -> Memory a -> (Bool, Memory a)
touch a m = let seen = Set.member a m in (seen, Set.insert a m)

findRepeated :: Ord a => [a] -> a
findRepeated xs = find Set.empty $ xs
    where find m (x:xs) = let (seen, m') = touch x m
                          in if seen then x else find m' xs

main = do
    lines <- readInput 1
    let input = map parseInt lines
    print $ solve 1 input
    print $ solve 2 input