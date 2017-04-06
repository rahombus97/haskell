import BinaryTrees

height :: BTree a -> Int
height Empty = -1 
height (BNode v left right) =  1 + max (height left) (height right) 

autumn :: BTree a -> BTree a
autumn Empty = Empty
autumn (BNode v Empty Empty) = Empty
autumn (BNode v left right) = BNode v (autumn left) (autumn right)

full :: BTree a -> Bool 
full Empty = True
full (BNode v (BNode _ _ _) Empty) = False
full (BNode v Empty (BNode _ _ _)) = False
full (BNode v left right) = (full left) && (full right)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (BNode v left right) = BNode v (mirror right) (mirror left)

mapTree :: (a -> b) -> BTree a -> BTree b
mapTree f Empty = Empty
mapTree f (BNode v left right) = BNode (f v) (mapTree f left) (mapTree f right)

depthVals :: Int -> BTree a -> [a]
depthVals n Empty = []
depthVals n (BNode v left right)
          | n < 0 = []
          | n == 0 = [v] 
          | otherwise = (depthVals (n-1) left) ++ (depthVals (n-1) right)

minValue :: Ord a => a -> BTree a -> a
minValue val Empty = val
minValue val (BNode v left right) = minimum (v : ([minValue v left] ++ [minValue v right]))

onPath :: Path -> BTree a -> [a]
onPath path Empty = []
onPath [] (BNode v _ _) = [v]
onPath (Lft:xs) (BNode v left right) = v : onPath xs left
onPath (Rght:xs) (BNode v left right) = v : onPath xs right





         
         
        






