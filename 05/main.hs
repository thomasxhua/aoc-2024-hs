splitOnChar :: Char -> String -> [String]
splitOnChar c s = reverse $ helper s [] []
    where
        helper [] [] acc   = acc
        helper [] part acc = part:acc
        helper (x:xs) part acc 
            | x == c    = helper xs [] (part:acc)
            | otherwise = helper xs (part++[x]) acc

findSubstr :: String -> String -> Int
findSubstr subs s = helper subs s []
    where
        helper [] s' subs = (length s) - (length s') - (length subs)
        helper _ [] _     = -1
        helper (y:ys) (x:xs) state
          | x == y         = helper ys xs $ state++[x]
          | x == head subs = helper subs (x:xs) []
          | otherwise      = helper subs xs []

splitOnSubstr :: String -> String -> (String,String)
splitOnSubstr subs s = (take n s, drop (n+length subs) s)
    where
        n = findSubstr subs s

type Rule   = (Int,Int)
type Page   = Int
type Update = [Page]

isUpdateConformingRule :: Rule -> Update -> Bool
isUpdateConformingRule (a,b) u = case relevantPages of
    []     -> True
    (r:rs) -> not hasA || not hasB || (r == a)
    where
        relevantPages = filter (\x -> (x == a) || (x == b)) u
        hasA          = elem a relevantPages
        hasB          = elem b relevantPages

firstRuleBreak :: [Rule] -> Update -> Maybe Rule
firstRuleBreak [] u     = Nothing
firstRuleBreak (r:rs) u = if isUpdateConformingRule r u
                            then firstRuleBreak rs u
                            else Just r

isUpdateValid :: [Rule] -> Update -> Bool
isUpdateValid rs = (== Nothing) . firstRuleBreak rs

fixUpdateForRule :: Maybe Rule -> Update -> Update
fixUpdateForRule _ []      = []
fixUpdateForRule Nothing u = u
fixUpdateForRule (Just r@(a,b)) (x:xs) 
    | x == a    = b : fixUpdateForRule (Just r) xs
    | x == b    = a : fixUpdateForRule (Just r) xs
    | otherwise = x : fixUpdateForRule (Just r) xs

fixUpdate :: [Rule] -> Update -> Update
fixUpdate [] u = u
fixUpdate _ [] = []
fixUpdate rs u = case isUpdateValid rs u of
    True -> u
    False -> fixUpdate rs (fixUpdateForRule (firstRuleBreak rs u) u)

fixOnlyWrongUpdates :: [Rule] -> [Update] -> [Update]
fixOnlyWrongUpdates _ []      = []
fixOnlyWrongUpdates [] us     = us
fixOnlyWrongUpdates rs (u:us) = case isUpdateValid rs u of
    True  -> fixOnlyWrongUpdates rs us
    False -> fixUpdate rs u : fixOnlyWrongUpdates rs us

parseSections :: String -> ([Rule],[Update])
parseSections s = (map parseRule (splitOnChar '\n' a), map parseUpdate (splitOnChar '\n' b))
    where
        (a,b)       = splitOnSubstr "\n\n" s
        parseRule   = (\[a,b] -> (read a, read b)) . splitOnChar '|'
        parseUpdate = (map read) . splitOnChar ','

middleElement :: [a] -> Maybe a
middleElement [] = Nothing
middleElement l  = Just $ l !! (length l `div` 2)

justElements :: [Maybe a] -> [a]
justElements []     = []
justElements (x:xs) = case x of
                        Just x' -> x' : justElements xs
                        Nothing -> justElements xs

solve01 :: String -> String
solve01 input = show . sum . justElements . map middleElement $ filter (isUpdateValid rules) updates
    where
        (rules,updates) = parseSections input

solve02 :: String -> String
solve02 input = show . sum . justElements . map middleElement $ fixOnlyWrongUpdates rules updates
    where
        (rules,updates) = parseSections input

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve02 input

