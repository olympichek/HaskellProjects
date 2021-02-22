{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except ( MonadError(throwError) )
import Control.Arrow ( (^>>), (>>>), Kleisli(Kleisli, runKleisli) )


safeDiv :: MonadError String m => Double -> Double -> m Double
safeDiv a 0 = throwError "Division by zero!"
safeDiv a b = return $ a / b

safeSqrt :: MonadError String m => Double -> m Double
safeSqrt a
    | a < 0 = throwError "Sqrt from negative number!"
    | otherwise = return $ sqrt a

f :: MonadError String m => Double -> m Double
f = runKleisli $ Kleisli safeSqrt
    >>> Kleisli (safeDiv 2)
    >>> (+2)
    ^>> Kleisli (`safeDiv` 2)