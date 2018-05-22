import Data.Vect
import Debug.Trace

%default total

data StackCmd : Nat -> Nat -> Type -> Type where
  RunStack : (Vect inH Integer -> IO (a, Vect outH Integer)) ->
             StackCmd inH outH a

runStack : StackCmd inH outH a -> Vect inH Integer ->
           IO (a, Vect outH Integer)
runStack (RunStack f) v = f v

pure : a -> StackCmd inh inh a
pure a = RunStack $ \v => pure (a, v)

namespace StackCmd
  (>>=) : StackCmd inh outh a -> (a -> StackCmd outh outh2 b) ->
          StackCmd inh outh2 b
  (>>=) (RunStack vf) f = RunStack $ \v => do
    (a, v') <- vf v
    let RunStack vf' = f a
    vf' v'

push : Integer -> StackCmd inH (S inH) ()
push x = RunStack $ \v => pure ((), x :: v)

pop : StackCmd (S outH) outH Integer
pop = RunStack $ \(x :: xs) => pure (x, xs)

top : StackCmd (S h) (S h) Integer
top = RunStack $ \(x :: xs) => pure (x, x :: xs)

getString : StackCmd inH inH String
getString = RunStack $ \v => map (\line => (line, v)) getLine  

putString : String -> StackCmd inH inH ()
putString s = RunStack $ \v => map (\_ => ((), v)) (putStr s)

putLine : String -> StackCmd inH inH ()
putLine = putString . (++ "\n")

binaryOp : (Integer -> Integer -> Integer) ->
           StackCmd (S (S h)) (S h) ()
binaryOp f = do
  v1 <- pop
  v2 <- pop
  push $ v1 `f` v2

addition : StackCmd (S (S h)) (S h) ()
addition = binaryOp (+)

multi : StackCmd (S (S h)) (S h) ()
multi = binaryOp (*)

minus : StackCmd (S (S h)) (S h) ()
minus = binaryOp (-)

data Fuel = Dry | More (Lazy Fuel)

data StackIO : Nat -> Type where
  Bind : StackCmd h1 h2 a -> (a -> Inf (StackIO h2)) -> StackIO h1

runIO : StackIO h -> Vect h Integer -> Fuel -> IO ()
runIO _ _ Dry = pure ()
runIO (Bind cmd f) v (More fuel) = do
  (a, v') <- runStack cmd v
  runIO (f a) v' fuel

namespace StackIO

  (>>=) : StackCmd inh outh a -> (a -> Inf (StackIO outh)) -> StackIO inh
  (>>=) = Bind

data StackInput = Number Integer
                | Add

stringToInput : String -> Maybe StackInput
stringToInput str =
  if all isDigit (unpack str)
    then Just $ Number $ cast str
    else if toLower str == "add"
    then Just Add
    else Nothing

mutual
  tryAdd : StackIO height
  tryAdd {height = S (S h)} = do
    addition
    result <- top
    putLine $ show result
    stackCalc

  tryAdd = do
    putString "there aren't enough numbers to add"
    stackCalc

  stackCalc : StackIO height
  stackCalc = do
    putString "> "
    line <- getString
    case stringToInput (trim line) of
        Nothing => do putLine "invalid input"
                      stackCalc
        Just (Number x) => do push x
                              stackCalc
        Just Add => tryAdd

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = runIO stackCalc [] forever
