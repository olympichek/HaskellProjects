fibonacci :: Integer -> Integer
fibonacci n 
    | n > 0 = helper1 0 1 1
    | n < 0 = helper2 0 1 (-1)
    | otherwise = 0
    where
    helper1 a b k
        | k == n = b
        | otherwise = helper1 b (a + b) (k + 1)
    helper2 a b k
        | k == n = b
        | otherwise = helper2 b (a - b) (k - 1)
        
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA n = helper 1 2 3 2 where
    helper a b c k
        | k == n = c
        | otherwise = helper b c (c + b - 2 * a) (k + 1)
        
sumAndCount :: Integer -> (Integer, Integer)
sumAndCount x 
    | x > 0 = helper x (0, 0)
    | x < 0 = helper (-x) (0, 0)
    | otherwise = (0, 1)
    where
    helper 0 (s, n) = (s, n)
    helper x (s, n) = helper (x `div` 10) (s + x `rem` 10, n + 1)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + s) where
    n = 1000
    h = (b - a) / n
    s = sum [f (a + h*n) | n <- [1..n-1]]

perms :: [a] -> [[a]]
perms [] = [[]]
perms [a] = [[a]]
perms [a, b] = [[a, b], [b, a]]
perms xs = concatMap f ys where
    f (a, as) = map (a:) $ perms as
    ys = helper xs [] []
    helper [] _ rs = rs
    helper (a:as) bs rs = helper as (a:bs) $ (a, as ++ bs):rs

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

change :: (Ord a, Num a) => a -> [[a]]
change n | n <= 0 = [[]]
change n = [x:xs | x <- coins, xs <- change (n - x), sum (x:xs) == n]
    where coins = [2, 3, 7]

main :: IO ()
main = print . sumAndCount =<< readLn