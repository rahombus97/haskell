import Data.Char

-- Name: Christopher Chomicki
-- Section: M013
-- Email: cchomick@syr.edu

locate :: Eq a => a -> [a] -> [Int]
locate x ys = map fst (filter (\(a,b) -> b == x) (zip [1..] ys)) 

histogram :: [Int] -> String
histogram xs = concat (map f xs)
     where
       f :: Int -> String
       f n = replicate n '*' ++ "\n"

manyFuns :: [a -> b] -> a -> [b]
manyFuns fs v = map (\f -> f v) fs

mySort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
mySort p [] = []
mySort p (x:xs) = ins x (mySort p xs)
        where                     
          ins y []      = [y]
          ins y (z:zs)             
            | p y z     = y:z:zs
            | otherwise = z: (ins y zs)

isFixPt :: Eq a => (a -> a) -> a -> Bool
isFixPt f val = val == (f val) 

changeFirst :: (a -> Bool) -> a -> [a] -> [a]
changeFirst p val [] = []
changeFirst p val (x:xs) 
            | (p x)     = val : xs
            | otherwise = x : changeFirst p val xs



     

 

        
      
