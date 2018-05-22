import System.Concurrency.Channels

data Message = Add Nat Nat

adder : IO ()
adder = do Just senderChan <- listen 1
               | Nothing => adder
           Just (Add x y) <- unsafeRecv Message senderChan
               | Nothing => adder
           unsafeSend senderChan (x + y)
           adder

main : IO ()
main = do Just adderID <- spawn adder
              | Nothing => putStrLn "Spawn failed"
          Just chan <- connect adderID
              | Nothing => putStrLn "Connection failed"
          ok <- unsafeSend chan (Add 2 3)
          Just answer <- unsafeRecv Nat chan
              | Nothing => putStrLn "Send fail"
          printLn answer
