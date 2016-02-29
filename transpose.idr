module Main

import Data.Vect
import Data.Vect.zipWith

Matrix : Type -> Nat -> Nat -> Type
Matrix a n m = Vect (Vect a m) n

--transpose : Matrix a n m -> Matrix a m n
--transpose (x:xs)


main : IO ()
main = putStrLn (show "Still working on it...")
