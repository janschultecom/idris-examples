import Data.Vect
double : Num ty => ty -> ty
double x = x * x

add : Int -> Int -> Int
add x y = x + y

twice : (a -> a) -> a -> a
twice f x = f (f x)

quadruple : Num a => a -> a
quadruple = twice double


fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]
sixInts : Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]

let tenInts  = fourInts ++ sixInts
