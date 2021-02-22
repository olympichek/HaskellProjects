import Control.Monad ( filterM )
import Control.Monad.Writer
    ( runWriter, MonadWriter(tell), Writer )

keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False

powerset :: [a] -> [[a]]  
powerset = filterM $ const [True, False]

main :: IO ()
main = do
    let (a, b) = runWriter $ filterM keepSmall [9,1,5,2,10,3]
    print a
    mapM_ putStrLn b
    print $ powerset [1,2,3]