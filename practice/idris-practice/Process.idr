module Process

import System.Concurrency.Channels

%default total

data MessagePID : (i : request -> Type) -> Type where
  MkMessage : PID -> MessagePID i

data ProcessState = NoRequest | Sent | Complete

data Process : (i : request -> Type) -> Type ->
               (inState : ProcessState) -> (outState : ProcessState) ->
               Type where
  Request : (MessagePID service) -> (message : request) ->
            Process i (service message) s s
  Respond : ((message : request) -> Process i (i message) NoRequest NoRequest) ->
            Process i (Maybe request) s Sent
  Spawn : Process service () NoRequest Complete ->
          Process i (Maybe (MessagePID service)) s s
  Loop : Inf (Process i a NoRequest Complete) -> Process i a Sent Complete
  Action : IO a -> Process i a s s
  Pure : a -> Process i a s s
  (>>=) : Process i a s1 s2 -> (a -> Process i b s2 s3) -> Process i b s1 s3

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> Process i a sIn sOut -> IO (Maybe a)

run Dry _ = pure Nothing

run _ (Action io) = Just `map` io

run _ (Pure a) = pure $ Just a

run fuel (Spawn p) = do
  Just pid <- spawn (const () `map` run fuel p)
      | Nothing => pure (Just Nothing)
  pure $ Just $ Just $ MkMessage pid

run (More fuel) (Loop p) = run fuel p

run fuel (p >>= f) = do
  Just a <- run fuel p | Nothing => pure Nothing
  run fuel (f a)

run _ (Request {service} (MkMessage pid) message) = do
  Just chan <- connect pid | Nothing => pure Nothing
  ok <- unsafeSend chan message
  if ok then do Just x <- unsafeRecv (service message) chan
                    | Nothing => pure Nothing
                pure $ Just x
        else pure Nothing

run fuel (Respond {request} f) = do
  Just sendChan <- listen 1 | Nothing => pure (Just Nothing)
  Just message <- unsafeRecv request sendChan | Nothing => pure (Just Nothing)
  Just result <- run fuel (f message) | Nothing => pure Nothing
  unsafeSend sendChan result
  pure $ Just $ Just message
