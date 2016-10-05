import Data.Vect

read_vect_len : (len : Nat) -> IO (Vect len String)
read_vect_len Z = pure []
read_vect_len (S k) = do x <- getLine
                         xs <- read_vect_len k
                         pure (x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

read_vect : IO (len ** Vect len String)
read_vect = do x <- getLine
               if (x == "")
                  then pure (_ ** [])
                  else do (_ ** xs) <- read_vect
                          pure (_ ** x :: xs)

