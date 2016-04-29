module Main

import Data.Vect

Matrix :  Nat -> Nat -> Type -> Type
Matrix n m a = Vect n (Vect m a)

empties : Vect m (Vect 0 a)
empties {m = Z} = []
empties {m = (S k)} = [] :: empties

transpose_helper : (row : Vect m a) -> (row_trans : Vect m (Vect k a)) -> Vect m (Vect (S k) a)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

transpose_vec: Matrix n m a -> Matrix m n a
transpose_vec [] = empties
transpose_vec (row :: rows) = let row_trans = transpose_vec rows in
  transpose_helper row row_trans

mult_vects : Num a => Vect m a -> Vect m a -> a
mult_vects [] [] = 0
mult_vects (x :: xs) (y :: ys) = x * y + mult_vects xs ys

mult_vect_mat : Num a => Vect m a -> Matrix n m a -> Vect n a
mult_vect_mat {n = Z} xs [] = replicate Z 0
mult_vect_mat {n = (S k)} xs (y :: ys) = (mult_vects xs y) :: mult_vect_mat xs ys

mult_mat : Num a => Matrix l m a -> Matrix m k a -> Matrix l k a
mult_mat [] [] = []
mult_mat [] (x :: xs) = []
mult_mat (x :: xs) as = let transposed = transpose_vec as in
                           (mult_vect_mat x transposed) :: mult_mat xs as

testMatrixA : Matrix 4 3 Nat
testMatrixA = ( 1 :: 2 :: 3 :: Nil) :: ( 10 :: 20 :: 30 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

testMatrixB : Matrix 3 4 Nat
testMatrixB = ( 1 :: 2 :: 3 :: 4 :: Nil) :: ( 10 :: 20 :: 30 :: 40 :: Nil) :: (22 :: 33 :: 44 :: 55 :: Nil) :: Nil


main : IO ()
main = putStrLn (show (mult_mat testMatrixA testMatrixB))
