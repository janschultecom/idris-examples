module Main

import Data.Vect
import Vect.zipWith

Matrix : Type -> Nat -> Nat -> Type
Matrix a n m = Vect (Vect a m) n

transpose : Matrix a n m -> Matrix a m n
transpose 

main : IO ()
main = putStrLn (show 3)
