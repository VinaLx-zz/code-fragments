module FoldDir where

import ControlledVisit (Info, getInfo, getUsefulContents, isDirectory)
import System.FilePath ((</>))

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

-- use the seed and the info of new file to determin whether the recurse go on
type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
        where fold seed subpath = getUsefulContents subpath >>= walk seed
              walk seed (name : names) = do
                  let path' = path </> name -- full path in the directory
                  info <- getInfo path' -- get the info of the file
                  case iter seed info of
                      done @ (Done _) -> return done -- if iterator choose Done
                      -- skip the processing of current file
                      Skip seed' -> walk seed' names
                      Continue seed'
                          | isDirectory info -> do
                              next <- fold seed' path'
                              case next of
                                  done @ (Done _) -> return done
                                  seed'' -> walk (unwrap seed'') names
                          | otherwise -> walk seed' names -- same as skip
              -- always choose continue when current dir is over
              walk seed _ = return (Continue seed)
