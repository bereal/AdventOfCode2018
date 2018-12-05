import Data.Char
import Common

match a b = isLower a /= isLower b && toLower a == toLower b

replace [] = []
replace [x] = [x]
replace (x:y:xs) = if match x y then replace xs else x : replace (y:xs)

find (x:y:xs) = if length x == length y then x else find (y:xs)

solve 1 input = find $ iterate replace input

main = do
    input <- readInput 5
    print $ length $ solve 1 $ head input