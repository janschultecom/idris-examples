module Main

import Data.Vect
import Prelude.Nat

Matrix :  Nat -> Nat -> Type -> Type
Matrix n m a = Vect n (Vect m a)

empties : Vect m (Vect 0 a)
empties {m = Z} = []
empties {m = (S k)} = [] :: empties

transpose_helper : (row : Vect m a) -> (row_trans : Vect m (Vect k a)) -> Vect m (Vect (S k) a)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

--transpose_vec: Vect n (Vect m a) -> Vect m (Vect n a)
transpose_vec: Matrix n m a -> Matrix m n a
transpose_vec [] = empties
transpose_vec (row :: rows) = let row_trans = transpose_vec rows in
  transpose_helper row row_trans

testMatrix : Matrix 3 4 Nat
testMatrix = ( 1 :: 2 :: 3 :: 4 :: Nil) :: ( 10 :: 20 :: 30 :: 40 :: Nil) :: ( 100 :: 200 :: 300 :: 400 :: Nil) :: Nil

transposed : Matrix 4 3 Nat
transposed = transpose_vec testMatrix

main : IO ()
main = putStrLn (show testMatrix ++ "\n--after transpose--\n" ++ show transposed)
