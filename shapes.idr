data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Eq Shape where
  (==) (Triangle w h) (Triangle w' h') =
    w == w' && h == h'
  (==) (Rectangle w h) (Rectangle w' h') =
    w == w' && h == h'
  (==) (Circle r) (Circle r') =
    r == r'
  (==) _ _ = False

Ord Shape where
  compare first second = compare (area first) (area second)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture
%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)
circle : Picture
circle = Primitive (Circle 5)
triangle : Picture
triangle = Primitive (Triangle 10 10)
triangle2 : Picture
triangle2 = Primitive (Triangle 100 10)

test_picture : Picture
test_picture = Combine (Combine (Translate 5 5 rectangle)
                  (Combine (Translate 35 5 circle) (Translate 15 25 triangle))) (Translate 20 20 triangle2)


picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine pic1 pic2) = picture_area pic1 + picture_area pic2
picture_area (Rotate x pic) = picture_area pic
picture_area (Translate x y pic) = picture_area pic

safe_divide : Double -> Double -> Maybe Double
safe_divide x 0.0 = Nothing
safe_divide x y = Just (x / y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = case compare x y of
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x

biggestTriangle_helper : (p : Picture) -> (size : Maybe Double) -> Maybe Double
biggestTriangle_helper (Primitive tri @ (Triangle x y)) Nothing = Just (area tri)
biggestTriangle_helper (Primitive tri @ (Triangle x y)) right_size @ (Just z) = let left_size = Just (area tri) in
                                                                                    maxMaybe left_size right_size
biggestTriangle_helper (Primitive (Rectangle x y)) size = size
biggestTriangle_helper (Primitive (Circle x)) size = size
biggestTriangle_helper (Combine left right) size = let left_size = biggestTriangle_helper left size
                                                       right_size = biggestTriangle_helper right size in
                                                       maxMaybe (maxMaybe left_size right_size) size
biggestTriangle_helper (Rotate x pic) size = biggestTriangle_helper pic size
biggestTriangle_helper (Translate x y pic) size = biggestTriangle_helper pic size

biggestTriangle : Picture -> Maybe Double
biggestTriangle p = biggestTriangle_helper p Nothing
