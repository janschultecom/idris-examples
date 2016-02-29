module Main

import Data.Vect

vtake : {auto p: LTE n m} -> (n:Nat) -> Vect m a -> Vect n a
vtake Z v = Nil
vtake (S k) (x::xs) = x :: (vtake k xs)

main : IO ()
main = putStrLn (show (vtake 3 (1 :: 2 :: 3 :: 4 :: 5 :: Nil)))
