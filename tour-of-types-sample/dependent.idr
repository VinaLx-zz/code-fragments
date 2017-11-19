module Concat

import Data.Vect

concat : Vect n a -> Vect m a -> Vect (m + n) a
