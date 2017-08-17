import Data.Vect

readVectFile' : (file: File) -> IO (n ** Vect n String)
readVectFile' file = do
  Right line <- fGetLine file | Left _ => pure (_ ** [])
  eof <- fEOF file
  if eof then pure (_ ** [])
         else do
           (_ ** v) <- readVectFile' file
           pure (_ ** line :: v)

readVectFile : (filename: String) -> IO (n ** Vect n String)
readVectFile f = do
  Right file <- openFile f Read | Left err => pure (_ ** [])
  readVectFile' file

readVectFileAndPrint : (filename: String) -> IO ()
readVectFileAndPrint f = do
  (_ ** v) <- readVectFile f
  putStrLn (show v)

main : IO ()
main = readVectFileAndPrint "Guess.idr"
