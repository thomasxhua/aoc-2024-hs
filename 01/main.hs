firstSpace :: String -> Int
firstSpace s = helper s 0
    where
        helper [] _     = -1
        helper (x:xs) n = if x == ' ' then n else helper xs (n+1)

splitStr :: String -> (String,String)
splitStr s = (take n s, drop (n + length delimiter) s)
    where
        delimiter = "   "
        n         = firstSpace s

splitOnNewLine :: String -> [String]
splitOnNewLine s = reverse $ helper s [] []
    where
        helper [] [] acc       = acc
        helper [] line acc     = line:acc
        helper (x:xs) line acc 
            | x == '\n' = helper xs [] (line:acc)
            | otherwise = helper xs (line++[x]) acc

parseLinesToLists :: [String] -> ([Int], [Int])
parseLinesToLists lst = helper lst [] []
    where
        helper [] as bs     = (as,bs)
        helper (x:xs) as bs = helper xs (as++[read a]) (bs++[read b])
            where
                (a,b) = splitStr x

insertSort :: [Int] -> [Int]
insertSort []     = []
insertSort (x:xs) = helper $ insertSort xs
    where
        helper [] = [x]
        helper (y:ys)
            | x<y       = x:y:ys
            | otherwise = y : helper ys

solve02 :: String -> String
solve02 input = show . sum $ map similarityScore as
    where
        (as,bs) = parseLinesToLists . splitOnNewLine $ input
        similarityScore a = a * (length $ filter ((==) a) bs)

solve01 :: String -> String
solve01 input = show . sum . map abs $ (zipWith (-)) as' bs'
    where
        (as,bs)   = parseLinesToLists . splitOnNewLine $ input
        (as',bs') = (insertSort as, insertSort bs)

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve02 input

