import System.IO ( hFlush, stdout )
import Control.Monad ( (<=<) )

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c, r) = filter onBoard  
    [ (c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1)
    , (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
    ] where
    onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]
    
inN :: KnightPos -> Int -> [KnightPos]
inN start n = foldr1 (<=<) (replicate n moveKnight) start

canReachInN :: KnightPos -> KnightPos -> Int -> Bool
canReachInN start end n = end `elem` inN start n

main :: IO ()
main = do
    let putStrFlush s = putStr s >> hFlush stdout
        getPos = (map read . words) `fmap` getLine
    putStrFlush "Enter start coordinates: "
    [x1, y1] <- getPos
    putStrFlush "Enter end coordinates: "
    [x2, y2] <- getPos
    putStrFlush "Enter number of moves: "
    n <- readLn
    if canReachInN (x1, y1) (x2, y2) n
        then putStrLn "Yes!"
        else putStrLn "No!"
