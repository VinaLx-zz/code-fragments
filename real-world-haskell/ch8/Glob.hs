module Glob (namesMatching) where

import Control.Exception
    (handle)
import Control.Monad
    (forM)
import System.Directory
    (doesDirectoryExist, doesFileExist, getCurrentDirectory,
    getDirectoryContents)
import System.FilePath
    (dropTrailingPathSeparator, splitFileName, (</>))
import GHC.IO.Exception

import GlobRegex

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
    | not (isPattern pat) = do
        exists <- doesNameExist pat
        return [pat | exists]
    | otherwise =
        case splitFileName pat of
            ("", baseName) -> do
                curDur <- getCurrentDirectory
                listMatches curDur baseName
            (dirName, baseName) -> do
                dirs <- if isPattern dirName
                           then namesMatching (dropTrailingPathSeparator dirName)
                           else return [dirName]
                let listDir = if isPattern baseName
                    then listMatches
                    else listPlain
                pathNames <- forM dirs $ \dir -> do
                    baseNames <- listDir dir baseName
                    return (map (dir </>) baseNames)
                return  (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
       then return True
       else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                   then getCurrentDirectory
                   else return dirName
    -- what this type annotation should be?
    (handle :: (IOException -> IO [String]) -> IO [String] -> IO[String]) (const (return [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden :: String -> Bool
isHidden cs = case cs of
                  ('.' : _) -> True
                  _         -> False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
                 then doesDirectoryExist dirName
                 else doesNameExist (dirName </> baseName)
    return [baseName | exists]
