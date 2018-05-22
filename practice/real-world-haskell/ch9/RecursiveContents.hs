module RecursiveContents (getRecursiveContents) where

import Control.Monad
    (forM)
import System.Directory
    (doesDirectoryExist, getDirectoryContents)
import System.FilePath
    ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
    names <- getDirectoryContents topDir
    let properNames = (`notElem` [".", ".."]) `filter` names 
    paths <- forM properNames $ \name -> do
        let path = topDir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
           then getRecursiveContents path
           else return [path]
    return (concat paths)

