module Main

import Data.Vect

add : Int -> Int -> Int
add x y = x + y

xs : Vect 2 Int
xs = [10,20]

ys : Vect 0 Int
ys = []

ErrorOrValue : Bool -> Type
ErrorOrValue False = String
ErrorOrValue True = Double

div : (divisor:Nat) -> (dividend:Nat) -> ErrorOrValue (dividend /= Z)
div divisor Z = "Error: Dividend is zero"
div divisor dividend@(S k) = (cast divisor) / (cast dividend)
--div divisor (S k) = (cast divisor) / (cast (S k))


vtake : (n:Nat) -> Vect (n+m) a -> Vect n a
vtake Z xs = []
vtake (S k) (head::tail) = head :: vtake k tail

mylist : Vect 5 Int
mylist = [1,2,3,4,5]


main : IO ()
main = putStrLn (show (vtake 6 mylist))
