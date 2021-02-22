import Control.Monad.Writer
    ( Sum, (<=<), MonadWriter(tell), runWriter, Writer )

half :: Int -> Writer String Int
half x = do
    tell $ show x ++ " halved! "
    return $ x `div` 2

half' :: Int -> Writer (Sum Int) Int
half' x = do
    tell 1
    return $ x `div` 2    

main :: IO ()
main = do
    let (var, logs) = runWriter $ half <=< half $ 8
    print var
    putStrLn logs