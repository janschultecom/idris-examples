import Data.Vect

read_vect_len : (len : Nat) -> IO (Vect len String)
read_vect_len Z = pure []
read_vect_len (S k) = do x <- getLine
                         xs <- read_vect_len k
                         pure (x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a
