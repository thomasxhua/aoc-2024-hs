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
    | StateD
    | StateDO
    | StateDON
    | StateDONa
    | StateDONaT
    | StateDONaTb
    | StateDONaTbb
    | StateD'
    | StateDO'
    | StateDO'b
    | StateDO'bb
    deriving (Show, Eq, Ord, Enum)

autMulFirst :: StateMulFirst -> (String,String) -> String -> (Maybe (String,String), String)
autMulFirst state acc []       = case state of
    StateMULbncnb -> (Just acc,[])
    _             -> (Nothing,[])
autMulFirst state (a,b) (x:xs) = case state of
    State0        -> case x of
                       'm' -> autMulFirst StateM nil xs
                       'd' -> autMulFirst StateD nil xs
                       _   -> autMulFirst State0 nil xs
    -- don't cases
    StateD        -> acceptOrReject (x == 'o')
    StateDO       -> acceptOrReject (x == 'n')
    StateDON      -> acceptOrReject (x == '\'')
    StateDONa     -> acceptOrReject (x == 't')
    StateDONaT    -> acceptOrReject (x == '(')
    StateDONaTb   -> acceptOrReject (x == ')')
    StateDONaTbb  -> acceptOrRejectDont (x == 'd')
    -- (don't case =>) do cases
    StateD'       -> acceptOrRejectDont (x == 'o')
    StateDO'      -> acceptOrRejectDont (x == '(')
    StateDO'b     -> acceptOrRejectDont (x == ')')
    StateDO'bb    -> autMulFirst State0 nil (x:xs)
    -- mul cases
    StateM        -> acceptOrReject (x == 'u')
    StateMU       -> acceptOrReject (x == 'l')
    StateMUL      -> acceptOrReject (x == '(')
    StateMULb     -> acceptOrRejectNumLeft (succ state) False
    StateMULbn    -> acceptOrRejectNumLeft state (x == ',')
    StateMULbnc   -> acceptOrRejectNumRight (succ state) (isDigit x)
    StateMULbncn  -> acceptOrRejectNumRight state (x == ')')
    StateMULbncnb -> (Just (a,b),(x:xs))
    where
        nil = ([],[])
        acceptOrReject cond = if cond
            then autMulFirst (succ state) (a,b) xs
            else autMulFirst State0 nil (x:xs)
        acceptOrRejectNumLeft state' cond = if isDigit x
            then autMulFirst state' (a++[x],b) xs
            else acceptOrReject cond
        acceptOrRejectNumRight state' cond = if isDigit x
            then autMulFirst state' (a,b++[x]) xs
            else acceptOrReject cond
        acceptOrRejectDont cond = if cond
            then autMulFirst (succ state) (a,b) xs
            else autMulFirst StateDONaTbb (a,b) xs

parseMulFirst :: String -> (Maybe (Int,Int), String)
parseMulFirst s = case autMulFirst State0 ([],[]) s of
    (Nothing, rest)    -> (Nothing, rest)
    (Just (a,b), rest) -> (Just (read a, read b), rest)

parseMuls :: String -> [(Int,Int)]
parseMuls [] = []
parseMuls s  = case parseMulFirst s of
    (Nothing, rest)   -> parseMuls rest
    (Just nums, rest) -> nums : (parseMuls rest)

solve :: String -> String
solve = show . sum . (map (\(a,b) -> a*b)) . parseMuls

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve input

