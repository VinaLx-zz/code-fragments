module MonadStack where

import Control.Monad.Reader
import Control.Monad.State
import System.Directory
import System.FilePath

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
} deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k depth =
    runStateT (runReaderT k config) st
        where config = AppConfig depth
              st = AppState 0

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
    contents <- liftIO . listDirectory $ path
    config <- ask
    rest <- forM contents $ \name -> do
        let newPath = path </> name
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir && curDepth < cfgMaxDepth config
           then do
               let newDepth = curDepth + 1
               st <- get
               when (stDeepestReached st < newDepth) $
                   put st { stDeepestReached = newDepth }
               constrainedCount newDepth newPath
           else return []
    return $ (path, length contents) : concat rest
