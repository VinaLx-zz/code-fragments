module ConsoleIO

import InfIO

%default total

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  Pure : a -> Command a
  Bind : Command a -> (a -> Command b) -> Command b

(>>=) : Command a -> (a -> Command b) -> Command b
(>>=) = Bind

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

runCommand : Command a -> IO a
runCommand (PutStr s) = putStr s
runCommand GetLine = getLine
runCommand (Pure a) = pure a
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)
