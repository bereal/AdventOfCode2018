module Common where

readInput:: Int -> IO [String]
readInput day = do
    let filename = concat ["input_", show day, ".txt"]
    content <- readFile filename
    return $ lines content