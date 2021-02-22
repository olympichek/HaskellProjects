import System.Environment ( getArgs )
import System.Directory ( doesFileExist )  

countLines :: String -> IO Int
countLines fileName = do
    contents <- readFile fileName
    return . length . lines $ contents

main :: IO ()
main = do
    fileName:_ <- getArgs  
    fileExists <- doesFileExist fileName  
    if fileExists then do
        n <- countLines fileName
        putStrLn $ "The file has " ++ show n ++ " lines!"  
    else putStrLn "The file doesn't exist!"  