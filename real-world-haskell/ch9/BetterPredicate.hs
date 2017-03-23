module BetterPredicate where

import Control.Exception
    (handle)
import Control.Monad
    (filterM)
import Data.Time.Clock
    (UTCTime)
import GHC.IO.Exception
    (IOException)
import System.Directory
    (Permissions (..), getModificationTime, getPermissions)
import System.IO
    (IOMode (..), hFileSize, withFile)

import RecursiveContents
    (getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe Integer)) $
    withFile path ReadMode $ \h -> do
        size <- hFileSize h
        return $ Just size

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP, equalP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
equalP = liftP (==)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = lftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w
