splitOnChar :: Char -> String -> [String]
splitOnChar c s = reverse $ helper s [] []
    where
        helper [] [] acc   = acc
        helper [] part acc = part:acc
        helper (x:xs) part acc 
            | x == c    = helper xs [] (part:acc)
            | otherwise = helper xs (part++[x]) acc

charGuardUp = '^'

data GuardDirection = GuardUp | GuardRight | GuardDown | GuardLeft
    deriving (Show, Eq, Enum)

succGuardDirection :: GuardDirection -> GuardDirection
succGuardDirection GuardLeft = GuardUp
succGuardDirection d         = succ d

type Posn      = (Int,Int)
type Obstacles = [Posn]
type Guard     = (Posn, GuardDirection)
type Level     = [String]

parseLevel :: String -> Level
parseLevel = splitOnChar '\n'

levelBounds :: Level -> (Int,Int)
levelBounds []  = (0,0)
levelBounds lvl = (length lvl, length $ head lvl)

isOutOfBounds :: Level -> Guard -> Bool
isOutOfBounds lvl ((x,y),_) = 0 > x || x >= width || 0 > y || y >= height
    where
        (width,height) = levelBounds lvl

generatePosns :: Level -> [Posn]
generatePosns lvl = [(x,y) | x <- [0..(length lvl)-1], y <- [0..(length (head lvl))-1]]

parseGuard :: Level -> Guard
parseGuard lvl = (findGuard $ generatePosns lvl, GuardUp)
    where
        findGuard [] = (-1,-1)
        findGuard ((a,b):xs)
            | (lvl !! a !! b) == charGuardUp = (a,b)
            | otherwise                      = findGuard xs
            
parseObstacles :: Level -> Obstacles
parseObstacles lvl = foldl appendIfObstacle [] (generatePosns lvl)
    where
        appendIfObstacle acc (a,b) = case (lvl !! a !! b) of
                                       '#' -> (a,b) : acc
                                       _   -> acc

type Set a = [a]

insertSet :: Eq a => a -> Set a -> Set a
insertSet x s
    | x `elem` s = s
    | otherwise  = x:s

calculateNextPosn :: Guard -> Posn
calculateNextPosn ((x,y),dir) = case dir of
    GuardUp    -> (x-1,y)
    GuardRight -> (x,y+1)
    GuardDown  -> (x+1,y)
    GuardLeft  -> (x,y-1)

calculateNextGuard :: Obstacles -> Guard -> Guard
calculateNextGuard o (p,d)
    | p' `elem` o = calculateNextGuard o (p,succGuardDirection d)
    | otherwise   = (p',d)
        where
            p' = calculateNextPosn (p,d)

generateSteps :: Level -> Guard -> Set Posn
generateSteps [] _      = []
generateSteps lvl guard = helper guard []
    where
        obstacles = parseObstacles lvl
        helper g@(posn, dir) acc
            | isOutOfBounds lvl g' = acc'
            | otherwise            = helper g' acc'
            where
                acc' = insertSet posn acc
                g'   = calculateNextGuard obstacles g

solve :: String -> String
solve input = show $ length $ generateSteps lvl guard
    where
        lvl   = parseLevel input
        guard = parseGuard lvl

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve input

