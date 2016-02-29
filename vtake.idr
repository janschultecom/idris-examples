module Main

import Data.Vect

vtake : (n:Nat) -> Vect (n+m) a -> Vect n a
vtake Z v = Nil
vtake (S k) (x::xs) = x :: (vtake k xs)

main : IO ()
main = putStrLn (show (vtake 6 (1 :: 2 :: 3 :: 4 :: 5 :: Nil)))
