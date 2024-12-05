splitOnChar :: Char -> String -> [String]
splitOnChar c s = reverse $ helper s [] []
    where
        helper [] [] acc   = acc
        helper [] part acc = part:acc
        helper (x:xs) part acc 
            | x == c    = helper xs [] (part:acc)
            | otherwise = helper xs (part++[x]) acc

parse :: String -> [[Int]]
parse s = map (map read) $ map (splitOnChar ' ') (splitOnChar '\n' s)

diffLeft :: [Int] -> [Int]
diffLeft [] = []
diffLeft l  = tail $ tail $ fst $ foldl accDiff acc l
    where
        acc     = ([0], head l)
        accDiff = (\acc' x -> ((fst acc')++[x - snd acc'], x))

validFloor :: [Int] -> Bool
validFloor = ((&&) <$> allAtMostAbsThree <*> allMonotone) . diffLeft
    where
        allAtMostAbsThree = all (\x -> (abs x) <= 3)
        allMonotone       = (||) <$> all (> 0) <*> all (< 0)

countTrue :: [Bool] -> Int
countTrue = (foldl (\acc b -> acc + fromEnum b) 0)

solve01 :: String -> String
solve01 = show . countTrue . map validFloor . parse

removeNth :: Int -> [a] -> [a]
removeNth n l = take (n) l ++ drop (n+1) l

generateToleranceFloors :: [Int] -> [[Int]]
generateToleranceFloors l = l : map ((flip removeNth) l) [0..(length l)-1]

solve02 :: String -> String
solve02 = show . countTrue . map (or . map validFloor) . map generateToleranceFloors . parse

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve02 input

