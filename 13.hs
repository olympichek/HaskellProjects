import System.IO ( hFlush, stdout )
import Text.Read ( readMaybe )
import Control.Monad ( MonadPlus(mzero) )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )

putStrFlush :: String -> IO ()
putStrFlush s = putStr s >> hFlush stdout

getPositive :: IO (Maybe Int)
getPositive = do
    putStrFlush "Enter positive number: "
    maybeInt <- readMaybe <$> getLine
    case maybeInt of
        Just n -> if n > 0
            then return $ Just n
            else return Nothing
        _ -> return Nothing

getPositive' :: IO (Maybe Int)
getPositive' = runMaybeT $ do
    liftIO $ putStrFlush "Enter positive number: "
    n <- MaybeT $ readMaybe <$> getLine
    if n > 0 then return n else mzero

main :: IO ()
main = do
    maybePositive <- getPositive'
    case maybePositive of
        Just p -> print p
        _ -> putStrLn "Not a positive number!"