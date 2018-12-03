import Common
import Data.Maybe
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec

data Area = Area { areaId :: Int, left :: Int, top :: Int, width :: Int, height :: Int }
    deriving Show

enumArea :: Area -> [(Int, Int)]
enumArea a = let t = top a
                 l = left a
                 xs = [l..l + (width a) - 1]
                 ys = [t..t + (height a) - 1]
             in [(x, y) | x <- xs, y <- ys]

area :: GenParser Char st Area
area = do
    char '#'
    areaId_ <- integer
    spaces >> char '@' >> spaces
    left_ <- integer
    char ','
    top_ <- integer
    char ':' >> spaces
    w <- integer
    char 'x'
    h <- integer
    return $ Area areaId_ left_ top_ w h

integer = read <$> many1 digit

fromRight (Right a) = a

parseArea :: String -> Area
parseArea input = fromRight $ parse area "" input

countUse areas = countVals $ areas >>= enumArea

solve 1 areas = let count = countUse areas
                in length $ filter (>1) $ Map.elems count

solve 2 areas = let count = countUse areas
                    isGoodCell c = (fromJust $ Map.lookup c count) == 1
                    isGood a = all isGoodCell $ enumArea a
                in areaId $ head $ filter isGood areas


main = do
    input <- readInput 3
    let areas = map parseArea input
    print $ solve 1 areas
    print $ solve 2 areas