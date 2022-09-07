data STree a = Nil | Node (STree a) a (STree a) deriving (Eq, Ord, Show)

validPreorder :: Ord a => [a] -> Bool
validPreorder [] = True
validPreorder (a : as) = left ++ right == as && validPreorder left && validPreorder right
  where
    left = filter (< a) as
    right = filter (> a) as

auxFromPreorder :: Ord a => [a] -> STree a
auxFromPreorder [] = Nil
auxFromPreorder [a] = Node Nil a Nil
auxFromPreorder (a : as) = Node (auxFromPreorder left) a (auxFromPreorder right)
  where
    left = filter (< a) as
    right = filter (> a) as

fromPreorder :: Ord a => [a] -> Maybe (STree a)
fromPreorder as
  | not (validPreorder as) = Nothing
  | otherwise = Just (auxFromPreorder as)

mymin :: Ord a => STree a -> a
mymin (Node Nil x _) = x
mymin (Node tl _ _) = mymin tl

nextmin :: Ord a => STree a -> a
nextmin (Node Nil _ tr) = mymin tr -- we've hit the smallest, so the smallest number bigger than this is second smallest
nextmin (Node (Node Nil _ Nil) y _) = y -- we've hit the second smallest as there is only one element smaller than it
nextmin (Node tl _ _) = nextmin tl -- we've not hit the smallest or the second smallest. Answer must be in the left subtree

leafy :: STree a -> Int
leafy Nil = 0
leafy (Node Nil _ Nil) = 1
leafy (Node lt _ rt) = leafy lt + leafy rt

data LeafTree a = Leaf a | LNode (LeafTree a) (LeafTree a) deriving (Eq, Ord, Show)

lmax :: Ord a => LeafTree a -> a
lmax (Leaf c) = c
lmax (LNode lt rt) = lmax rt

lmin :: Ord a => LeafTree a -> a
lmin (Leaf c) = c
lmin (LNode lt rt) = lmin rt

hasSearch :: Ord a => LeafTree a -> Bool
hasSearch (Leaf a) = True
hasSearch (LNode lt rt) = hasSearch lt && hasSearch rt && lmax lt < lmin rt

search :: Ord a => LeafTree a -> a -> Bool
search (Leaf y) x = x == y
search (LNode lt rt) x
    | maxlt > x = search lt x
    | maxlt == x = True
    | otherwise = search rt x 
    where
        maxlt = lmax lt

