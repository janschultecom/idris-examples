module Main

import Data.Vect

test : Vect ?vlen String 
test = ["Scala","Prolog","Haskell","Idris","Curry"]

vlen = %runElab 

main : IO ()
main = putStrLn (show "Found length: \n" ++ ?vlen)
