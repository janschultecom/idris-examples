data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left value right) = case compare x value of
                                        LT => Node (insert x left) value right
                                        EQ => orig 
                                        GT => Node left value (insert x right)

insert_helper : Ord a => (x : a) -> Tree a -> Tree a
insert_helper x t = insert x t

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = let inserted = listToTree xs  in
                         insert_helper x inserted
 
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = let leftList  = (treeToList left) 
                                     rightList = x :: treeToList right 
                                     in leftList ++ rightList
