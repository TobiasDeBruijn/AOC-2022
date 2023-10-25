
module Main where
    main :: IO()
    main = do
        input <- readFile "1_2.txt"
        print $ sum $ getTopThreeCalories (sumElves $ parseLines (inputLines input) [] []) []

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

            getTopThreeCalories :: [Int] -> [Int] -> [Int]
            getTopThreeCalories [] buffer@[_, _, _] = buffer
            getTopThreeCalories [] buffer = undefined -- Shouldn't be hitting this
            getTopThreeCalories _ buffer@[_, _, _] = buffer
            getTopThreeCalories input buffer = getTopThreeCalories (filter(/= maxV) input) (maxV : buffer)
                where
                    maxV = maximum input