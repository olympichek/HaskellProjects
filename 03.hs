import Control.Monad.Reader ( asks, runReader, Reader )

hello :: Reader String String
hello = asks $ \name -> "hello, " ++ name ++ "! "

bye :: Reader String String
bye = asks $ \name -> "bye, " ++ name ++ "! "

convo :: Reader String String
convo = (++) <$> hello <*> bye

main :: IO ()
main = getLine >>= putStrLn . runReader convo