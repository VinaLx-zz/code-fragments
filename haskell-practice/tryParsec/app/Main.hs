module Main where

import Try.CSV

main :: IO ()
main = print $ parseCSV
    "header1,header2,header3\n\
    \cell1,cell2,cell3\n"
