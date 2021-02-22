import Data.Array.Unboxed ( UArray, (!), (//), indices, listArray )

getPrimes :: Int -> Int -> [Int]
getPrimes m n = filter (>= m) primes where
    boolArr :: UArray Int Bool
    boolArr = listArray (2, n) $ replicate (n-1) True
    notPrimes = [j | i <- [2..n], i*i <= n, j <- [i*i+k | k <- [0,i..n-i*i]]]
    boolArr' = boolArr // [(i, False) | i <- notPrimes]
    filterPrimes i is = if boolArr' ! i then i:is else is
    primes = foldr filterPrimes [] $ indices boolArr'

main :: IO ()
main = do
    [m, n] <- map read . words <$> getLine
    let primes = getPrimes m n
    case primes of
        [] -> putStrLn "Absent"
        _ -> mapM_ print primes