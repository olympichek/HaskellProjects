import Data.Monoid ( Sum(Sum) )
import Control.Monad.Writer
    ( MonadWriter(tell), runWriter, Writer )

import System.IO ( hFlush, stdout )

numberOfDigits :: Int -> Int 
numberOfDigits 0 = 1
numberOfDigits x = res where
    writer :: Int -> Writer (Sum Int) Int
    writer 0 = return 0
    writer x = do
        tell $ Sum 1
        writer $ x `div` 10
    (_, Sum res) = runWriter $ writer x

sumOfDigits :: Int -> Int 
sumOfDigits x = res where
    writer :: Int -> Writer (Sum Int) Int
    writer 0 = return 0
    writer x = do
        tell . Sum $ x `rem` 10
        writer $ x `div` 10
    (_, Sum res) = runWriter $ writer x

sumOfDigits' :: Int -> Int 
sumOfDigits' x = snd $ writer (x, 0) where
    writer (0, s) = (0, s)
    writer (x, s) = writer (x `div` 10, s + x `rem` 10)

main :: IO ()
main = do
    putStr "Enter number: " >> hFlush stdout
    x <- readLn
    let n = numberOfDigits x
        s = sumOfDigits x
        s' = sumOfDigits' x
    putStrLn $ "Number of digis: " ++ show n
    putStrLn $ "Sum of digis: " ++ show s
    putStrLn $ "Sum of digis': " ++ show s'