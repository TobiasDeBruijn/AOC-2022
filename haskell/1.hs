
module Main where
    main :: IO()
    main = do
        input <- readFile "1.txt"
        print $ getMaxCalories . sumElves $ parseLines (inputLines input) [] []
        
        where
            inputLines :: String -> [String]
            inputLines input = lines input
            
            parseLines :: [String] -> [Int] -> [[Int]] -> [[Int]]
            parseLines [] elfBuffer totalBuffer = elfBuffer : totalBuffer
            parseLines (line : lines) elfBuffer totalBuffer = case parseLine line of
                Nothing -> parseLines lines [] (elfBuffer : totalBuffer)
                Just v -> parseLines lines (v : elfBuffer) totalBuffer

            sumElves :: [[Int]] -> [Int]
            sumElves = map sum

            parseLine :: String -> Maybe Int
            parseLine v
                | v == "" = Nothing
                | otherwise = Just $ read v
            
            getMaxCalories :: [Int] -> Int
            getMaxCalories = maximum