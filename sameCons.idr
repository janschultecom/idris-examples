import Data.Vect

same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists  Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  Trans : (x:a) -> ThreeEq x x x 

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Trans z) = Trans (S z)


myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x]) where
                      reverseProof : Vect (len + 1) elem -> Vect (S len) elem
                      reverseProof {len} result = rewrite plusCommutative 1 len in result

append_rhs_2 : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_rhs_2 {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append {m} [] ys = rewrite plusZeroRightNeutral m in ys
append (x :: xs) ys = append_rhs_2 (x :: append xs ys)

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = let result = plusZeroRightNeutral m in
                        sym result 
myPlusCommutes (S k) m = let result = plusSuccRightSucc k m 
                             symresult = sym result
                         in
                                     ?usCommutes_rhs_2
