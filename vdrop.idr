module Main

import Data.Vect

vdrop : (n:Nat) -> Vect (n+m) a -> Vect m a
vdrop Z v = v
vdrop (S k) (x::xs) = vdrop k xs

main : IO ()
-- BOOOM!
main = putStrLn (show (vdrop 6 (1 :: 2 :: 3 :: 4 :: 5 :: Nil)))
