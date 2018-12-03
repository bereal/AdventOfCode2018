module Common where
import qualified Data.Map as Map

readInput:: Int -> IO [String]
readInput day = do
    let filename = concat ["input_", show day, ".txt"]
    content <- readFile filename
    return $ lines content


type Counter a = Map.Map a Int

increment :: Ord a => a -> Counter a -> Counter a
increment a c = Map.insert a (1 + (Map.findWithDefault 0 a c)) c

countVals :: Ord a => [a] -> Counter a
countVals = foldl (flip increment) Map.empty
