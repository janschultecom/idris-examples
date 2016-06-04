
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys


data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

read_vect : IO (VectUnknown String)
read_vect = do x <- getLine
               if (x == "")
                  then pure (MkVect _ [])
                  else do MkVect _ xs <- read_vect
                          pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs)
  = putStrLn (show xs Strings.++ " (length " Strings.++ show len Strings.++ ")")
