import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "false"
getStringOrInt True = 1

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = ?valToString_rhs_2
valToString True y = ?valToString_rhs_3
