splitOnChar :: Char -> String -> [String]
splitOnChar c s = reverse $ helper s [] []
    where
        helper [] [] acc   = acc
        helper [] part acc = part:acc
        helper (x:xs) part acc 
            | x == c    = helper xs [] (part:acc)
            | otherwise = helper xs (part++[x]) acc

isDigit :: Char -> Bool
isDigit = (&&) <$> (>= '0') <*> (<= '9')

data StateMulFirst
    = State0
    | StateM
    | StateMU
    | StateMUL
    | StateMULb
    | StateMULbn
    | StateMULbnc
    | StateMULbncn
    | StateMULbncnb
    deriving (Show, Eq, Ord, Enum)

autMulFirst :: StateMulFirst -> String -> String -> (String, String)
autMulFirst state acc []     = case state of
    StateMULbncnb -> (acc,[])
    _             -> ([],[])
autMulFirst state acc (x:xs) = case state of
    State0        -> case x of
                       'm' -> autMulFirst StateM [x] xs
                       _   -> autMulFirst State0 [] xs
    StateM        -> acceptOrReject (x == 'u')
    StateMU       -> acceptOrReject (x == 'l')
    StateMUL      -> acceptOrReject (x == '(')
    StateMULb     -> acceptOrReject (isDigit x)
    StateMULbn    -> acceptOrRejectNum (x == ',')
    StateMULbnc   -> acceptOrReject (isDigit x)
    StateMULbncn  -> acceptOrRejectNum (x == ')')
    StateMULbncnb -> (acc,(x:xs))
    where
        acceptOrReject cond
            | cond      = autMulFirst (succ state) (acc++[x]) xs
            | otherwise = autMulFirst State0 acc (x:xs)
        acceptOrRejectNum cond
            | isDigit x = autMulFirst state (acc++[x]) xs
            | otherwise = acceptOrReject cond

detectMulFirst :: String -> (String,String)
detectMulFirst = autMulFirst State0 []

detectMultStrings :: String -> [String]
detectMultStrings [] = []
detectMultStrings s  = case detectMulFirst s of
    ([],rest)     -> detectMultStrings rest
    (mulStr,rest) -> mulStr : (detectMultStrings rest)

solve :: String -> String
solve input = foldl (++) [] $ detectMultStrings input

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve input

