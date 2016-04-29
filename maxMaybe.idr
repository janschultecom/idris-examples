maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = case compare x y of 
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x

