module Expr

data Expr : Type where
  Value : (a:Int) -> Expr 
  Plus : Expr -> Expr -> Expr
  Minus : Expr -> Expr -> Expr
  Mult : Expr -> Expr -> Expr

evaluate : Expr -> Int
evaluate (Value x) = x
evaluate (Plus x y) = (evaluate x) + (evaluate y)
evaluate (Minus x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)
