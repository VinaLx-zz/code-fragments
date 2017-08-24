module InfIO

%default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More $ tank k

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) =
  do res <- c
     run fuel $ f res
run Dry p = putStrLn "out of fuel"

partial
forever : Fuel
forever = More forever

promptAndRead : String -> IO String
promptAndRead prompt = do putStrLn prompt
                          getLine

processLine : String -> (String -> String) -> String -> Inf InfIO
processLine prompt action str =
  Do (putStrLn (action str) >>= \_ => promptAndRead prompt) $ Delay (processLine prompt action)

totalRepl : (prompt : String) -> (action : String -> String) -> InfIO
totalRepl prompt action =
  Do (promptAndRead prompt) (processLine prompt action)
