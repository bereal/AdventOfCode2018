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

area = do
    id <- char '#' >> integer
    left <- spaces >> char '@' >> spaces >> integer
    top <- char ',' >> integer
    w <- char ':' >> spaces >> integer
    h <- char 'x' >> integer
    return $ Area id left top w h

integer = read <$> many1 digit

fromRight (Right a) = a

parseArea :: String -> Area
parseArea input = fromRight $ parse area "" input

countOverlap areas = countVals $ areas >>= enumArea

solve 1 areas = let count = countOverlap areas
                in length $ filter (>1) $ Map.elems count

solve 2 areas = let count = countOverlap areas
                    isGoodCell c = (fromJust $ Map.lookup c count) == 1
                    isGood a = all isGoodCell $ enumArea a
                in areaId $ head $ filter isGood areas


main = do
    input <- readInput 3
    let areas = map parseArea input
    print $ solve 1 areas
    print $ solve 2 areas