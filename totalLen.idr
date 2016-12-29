totalLen : List String -> Nat
totalLen xs = foldr (\elem, acc => length elem + acc ) 0 xs
