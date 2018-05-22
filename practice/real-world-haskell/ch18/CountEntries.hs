module CountEntries where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (forM_, when)
import Control.Monad.Writer (WriterT, tell, liftIO)
import System.FilePath ((</>))

listDirectory :: FilePath -> IO [String]
listDirectory = fmap (filter (`notElem` [".", ".."])) . getDirectoryContents

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries newName
