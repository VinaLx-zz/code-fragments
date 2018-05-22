import System

readInt : IO (Maybe Integer)
readInt = do
  line <- getLine
  let line' = trim line
  if all isDigit (unpack line')
     then pure (Just (cast line'))
     else pure Nothing

||| number guessing game
||| @ target the answer
guess : (target : Integer) -> IO ()
guess n = do
  putStr "number: "
  Just num <- readInt | Nothing => do
    putStrLn "A number plz!"
    guess n
  if n == num
     then putStrLn "Correct!"
     else if num < n
     then putStrLn "too small!" >>= \_ => guess n
     else putStrLn "too large!" >>= \_ => guess n

main : IO ()
main = do
  t <- time
  guess (mod t 100)
