module Main where

import System.IO (isEOF)
import Try.CSV
import Try.JSON

tryCSV :: IO ()
tryCSV = print $ parseCSV
    "header1,header2,header3\n\
    \cell1,cell2,cell3\n"

getAll :: IO String
getAll = do
    done <- isEOF
    if done
        then return ""
        else do
            line <- getLine
            rest <- getAll
            return $ line ++ rest


tryJson :: IO ()
tryJson = do
    putStrLn "Json plz:"
    json <- getAll
    print $ parseJson json

main :: IO ()
main = tryJson
