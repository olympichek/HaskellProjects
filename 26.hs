import Control.Monad ( (>=>) )

data Log a = Log [String] a
    deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg a = Log [msg] $ f a

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (m1 ++ m2) r2 where
    (Log m1 r1) = f x
    (Log m2 r2) = g r1

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log ls a) k = Log (ls ++ ls') r
    where (Log ls' r) = k a

instance Functor Log where
    fmap = undefined

instance Applicative Log where
    pure = undefined
    (<*>) = undefined

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

execLoggersList' :: a -> [a -> Log a] -> Log a
execLoggersList' = flip $ foldl1 (>=>)