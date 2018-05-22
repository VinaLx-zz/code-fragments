module ControlledVisit where

import Control.Exception (handle)
import Control.Monad     (forM, liftM)
import Data.Time.Clock   (UTCTime)
import GHC.IO.Exception  (IOException)
import System.FilePath   ((</>))
import System.IO         (IOMode (..), hFileSize, withFile)
import System.Directory
    ( Permissions
    , getDirectoryContents
    , getModificationTime
    , getPermissions
    , searchable
    )

data Info = Info {
    infoPath    :: FilePath,
    infoPerms   :: Maybe Permissions,
    infoSize    :: Maybe Integer,
    infoModTime :: Maybe UTCTime
}

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- getInfo `mapM` (path : map (path </>) names)
    fmap concat $ forM (order contents) $ \info ->
        if isDirectory info && infoPath info /= path
           then ControlledVisit.traverse order (infoPath info)
           else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO $ withFile path ReadMode hFileSize
    modified <- maybeIO $ getModificationTime path
    return $ Info path perms size modified

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle
    ((\_ -> return Nothing) :: IOException -> IO (Maybe a)) (Just `liftM` act)

filterTraverse :: (Info -> Bool) -> FilePath -> IO [Info]
filterTraverse f = ControlledVisit.traverse $ filter f
