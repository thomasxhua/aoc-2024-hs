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

solve01 :: String -> String
solve01 = show . xmasSum . (splitOnChar '\n')

get :: [String] -> (Int,Int) -> Char
get l (x,y) = (!! y) . (!! x) $ l

allowedXs :: [String]
allowedXs = ["MSAMS","MMASS","SSAMM","SMASM"]

generateXs :: [String] -> [String]
generateXs [] = []
generateXs l  = map generateX $ [(x,y) | x <- [0..m-2], y <- [0..n-2]]
    where
        m = length l-1
        n = length (head l)-1
        generateX (x,y) = map (get l) posns
            where
                posns = [(x,y), (x,y+2), (x+1,y+1), (x+2,y), (x+2,y+2)]

solve02 :: String -> String
solve02 = show . length . (filter (flip elem $ allowedXs)) . generateXs . (splitOnChar '\n')

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve02 input

