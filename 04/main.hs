splitOnChar :: Char -> String -> [String]
splitOnChar c s = reverse $ helper s [] []
    where
        helper [] [] acc   = acc
        helper [] part acc = part:acc
        helper (x:xs) part acc 
            | x == c    = helper xs [] (part:acc)
            | otherwise = helper xs (part++[x]) acc

xmas :: String
xmas = "XMAS"

xmasLength :: Int
xmasLength = length xmas

countRow :: String -> Int
countRow s = length $ filter (\w -> w == xmas) $ map (\i -> take xmasLength . drop i $ s) [0..(length s)-xmasLength]

xmasLtoR :: [String] -> Int
xmasLtoR = sum . map countRow

xmasRtoL :: [String] -> Int
xmasRtoL = xmasLtoR . map reverse

countCol :: Int -> [String] -> Int
countCol i = countRow . map (!! i)

xmasTtoB :: [String] -> Int
xmasTtoB [] = 0
xmasTtoB l  = sum . map (\f -> f l) . map countCol $ [0..(length (head l))-1]

xmasBtoT :: [String] -> Int
xmasBtoT = xmasTtoB . reverse

diagonalizeTtoL :: [String] -> [String]
diagonalizeTtoL [] = []
diagonalizeTtoL ls = helper ls []
    where
        helper [] acc     = acc
        helper (x:xs) acc = case acc of
            []     -> helper xs (map (:[]) x)
            (a:as) -> a : (helper xs $ [t++s | (t,s) <- zip as (map (:[]) x)]++[[last x]])

xmasTtoL :: [String] -> Int
xmasTtoL = xmasLtoR . diagonalizeTtoL

xmasLtoT :: [String] -> Int
xmasLtoT = xmasLtoR . (map reverse) . diagonalizeTtoL

xmasTtoR :: [String] -> Int
xmasTtoR = xmasTtoL . (map reverse)

xmasRtoT :: [String] -> Int
xmasRtoT = xmasLtoT . (map reverse)

xmasSum :: [String] -> Int
xmasSum l = sum $ map (\f -> f l)
    [ xmasLtoR, xmasRtoL
    , xmasTtoB, xmasBtoT
    , xmasTtoL, xmasLtoT
    , xmasTtoR, xmasRtoT
    ]

solve :: String -> String
solve = show . xmasSum . (splitOnChar '\n')

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve input

