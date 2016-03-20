module Main

import Data.Vect

repeat : (n : Nat) -> a -> Vect n a
repeat Z _ = Nil
repeat (S k) x = x :: (repeat k x) 

main : IO ()
main = putStrLn (show (repeat 4 "a"))
