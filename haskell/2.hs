module Main where 
    data Move = Rock | Paper | Scissors deriving (Eq, Show)

    parseMove :: String -> Move
    parseMove v 
        | v == "A" || v == "X" = Rock
        | v == "B" || v == "Y" = Paper
        | v == "C" || v == "Z" = Scissors 
        | otherwise = error "Invalid move"

    moveScore :: Move -> Int
    moveScore m
        | m == Rock         = 1
        | m == Paper        = 2
        | m == Scissors     = 3
        | otherwise         = error "Somehow we got here"
    
    roundScore :: (Move, Move) -> Int
    roundScore (a, b)
        | a == Rock && b == Scissors        = 0
        | a == Paper && b == Rock           = 0
        | a == Scissors && b == Paper       = 0
        | a == Rock && b == Rock            = 3
        | a == Paper && b == Paper          = 3
        | a == Scissors && b == Scissors    = 3
        | a == Scissors && b == Rock        = 6
        | a == Rock && b == Paper           = 6
        | a == Paper && b == Scissors       = 6
        | otherwise = error $ "Invalid combination " ++ show a ++ " with " ++ show b

    totalScoreForPair :: (Move, Move) -> Int
    totalScoreForPair p@(a, b) = moveScore b + roundScore p

    main :: IO()
    main = do
        input <- readFile "2.txt"

        print $ sum $ scoreLines $ parseLines $ inputLines input
        where
            inputLines :: String -> [String]
            inputLines v = lines v

            splitLines :: [String] -> [(String, String)]
            splitLines = map splitLine

            splitLine :: String -> (String, String)
            splitLine [a, _, b] = ([a], [b]) 
            splitLine _ = error "Invalid line"

            parseLine :: (String, String) -> (Move, Move)
            parseLine (a, b) = (parseMove a, parseMove b)

            parseLines :: [String] -> [(Move, Move)]
            parseLines = map $ parseLine . splitLine

            scoreLines :: [(Move, Move)] -> [Int]
            scoreLines = map totalScoreForPair

