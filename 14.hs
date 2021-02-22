import System.IO ( stdout, hFlush )
import Text.Read ( readMaybe )

import Control.Monad ( MonadPlus(mzero), when )
import Control.Monad.Trans ( MonadIO(liftIO) )

import Data.Maybe ( isNothing )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )

putStrFlush :: String -> IO ()
putStrFlush s = putStr s >> hFlush stdout

getTwoNumbers :: MonadPlus m => IO (m (Double, Double))
getTwoNumbers = do
    putStrFlush "Enter a and b: "
    inputs <- map readMaybe . words <$> getLine
    case inputs of
        [Just a, Just b] -> return $ return (a, b)
        _ -> return mzero

safeDiv :: MonadPlus m => Double -> Double -> m Double
safeDiv a 0 = mzero
safeDiv a b = return $ a / b

safeSqrt :: MonadPlus m => Double -> m Double
safeSqrt a
    | a < 0 = mzero
    | otherwise = return $ sqrt a

main :: IO ()
main = do
    res <- runMaybeT $ do
        (a, b) <- MaybeT getTwoNumbers
        aDivB <- a `safeDiv` b
        liftIO . putStrLn $ "a / b = " ++ show aDivB
        sqrtA <- safeSqrt a
        liftIO . putStrLn $ "sqrt a = " ++ show sqrtA
        sqrtB <- safeSqrt b
        liftIO . putStrLn $ "sqrt b = " ++ show sqrtB
    when (isNothing res) $ putStrLn "Error!"