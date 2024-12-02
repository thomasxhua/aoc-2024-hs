solve :: String -> String
solve input = input

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ solve input

