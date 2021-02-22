{-# LANGUAGE FlexibleContexts #-}

import System.IO ( stdout, hFlush )
import Text.Read ( readMaybe )

import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Except
    ( ExceptT(ExceptT), MonadError(throwError), runExceptT )

getTwoNumbers :: MonadError String m => IO (m (Double, Double))
getTwoNumbers = do
    putStr "Enter a and b: " >> hFlush stdout
    inputs <- map readMaybe . words <$> getLine
    case inputs of
        [Just a, Just b] -> return $ return (a, b)
        _ -> return $ throwError "Incorrect input!"

safeDiv :: MonadError String m => Double -> Double -> m Double
safeDiv a 0 = throwError "Division by zero!"
safeDiv a b = return $ a / b

safeSqrt :: MonadError String m => Double -> m Double
safeSqrt a
    | a < 0 = throwError "Sqrt from negative number!"
    | otherwise = return $ sqrt a

main :: IO ()
main = do
    error <- runExceptT $ do
        (a, b) <- ExceptT getTwoNumbers
        aDivB <- a `safeDiv` b
        liftIO . putStrLn $ "a / b = " ++ show aDivB
        sqrtA <- safeSqrt a
        liftIO . putStrLn $ "sqrt a = " ++ show sqrtA
        sqrtB <- safeSqrt b
        liftIO . putStrLn $ "sqrt b = " ++ show sqrtB
    either putStrLn return error