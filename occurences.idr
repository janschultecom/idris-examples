data Matter = Solid | Liquid | Gas

Eq Matter where
    (==) Solid Solid = True
    (==) Liquid Liquid = True
    (==) Gas Gas = True
    (==) _ _ = False
    (/=) x y = not (x == y)

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
    (==) Empty Empty = True
    (==) (Node left elem right) (Node 'left 'elem 'right) =
      left == left' && elem == 'elem && right == 'right
    (==) _ _ = False

occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  True => S (occurrences item xs)
                                  False => occurrences item xs
