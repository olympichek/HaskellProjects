input :: IO ([Int], [Int])
input = do
    putStrLn "Enter first list:"
    list1 <- map read . words <$> getLine
    putStrLn "Enter second list: "
    list2 <- map read . words <$> getLine
    return (list1, list2)

calcSum :: [Int] -> [Int] -> [Int]
calcSum list1 list2 = (+) <$> list1 <*> list2

output :: [Int] -> IO ()
output list = do
    mapM_ (\x -> putStr $ show x ++ " ") list
    putStrLn ""

main :: IO ()
main = input >>= output . uncurry calcSum