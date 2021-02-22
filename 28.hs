import System.Directory ( getDirectoryContents, removeFile )
import Data.List ( isInfixOf )

main :: IO ()
main = do
    putStr "Substring: "
    str <- getLine
    if str == "" then putStrLn "Canceled"
    else do
        files <- getDirectoryContents "."
        let toDelete = isInfixOf str <$> files
            filesToDelete = map fst . filter snd $ zip files toDelete
            msgs = map ("Removing file: " ++) filesToDelete
        mapM_ putStrLn msgs
        -- mapM_ removeFile filesToDelete

    