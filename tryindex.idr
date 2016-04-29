import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         (Just idx) => Just (index idx xs)

sumEntries : (Num a) => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = let left = tryIndex pos xs
                               right = tryIndex pos ys
                               in case (left,right) of
                                       (Just x,Just y) => Just (x+y)
                                       _ => Nothing

