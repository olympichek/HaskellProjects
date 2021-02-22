import System.Directory ( doesDirectoryExist, getDirectoryContents, listDirectory )
import System.FilePath ( (</>) )
import Control.Monad ( forM, forM_, when )

import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Writer ( WriterT, tell )

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    contents <- listDirectory path
    rest <- forM contents $ \name -> do
        let newName = path </> name
        isDir <- doesDirectoryExist newName
        if isDir
            then countEntriesTrad newName
            else return []
    return $ (path, length contents) : concat rest

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries newName