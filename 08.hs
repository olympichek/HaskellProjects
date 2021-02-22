import Control.Monad.Reader

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

hello' :: String -> String
hello' name = "hello, " ++ name ++ "!"

bye' :: String -> String
bye' name = "bye, " ++ name ++ "!"

convo' :: String -> String
convo' = do
    c1 <- hello'
    c2 <- bye'
    return $ c1 ++ c2

main :: IO ()
main = do
    print $ runReader convo "adit"
    print $ convo' "adit"