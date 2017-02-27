import Data.Char

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (n:ns) = n * myProduct ns

shout :: String -> String
shout []     = ""
shout (c:cs) = toUpper c : shout cs 
                  
zap :: Char -> String -> String
zap c [] = ""
zap c (x:xs)
      | c == x     =  zap c xs
      | otherwise  =  x : zap c xs

pairUp :: [a] -> [(a,a)]
pairUp []        = []
pairUp (x:y:xs)  = (x,y) : pairUp xs
pairUp (x:xs)    = (x,x) : pairUp xs

neighbors :: [a] -> [(a,a)]
neighbors []       = []
neighbors [a]      = []
neighbors (x:y:xs) = (x,y) : neighbors (y:xs)

bag1, bag2, bag3 :: [(Char, Int)]
bag1 = [('z',1), ('e',2), ('k',1)]
bag2 = [('y',2), ('a',1), ('n',1), ('c',1), ('e',1)]
bag3 = [('j',1), ('o',1), ('u',1), ('l',1), ('e',1)]

bagCount :: [(Char,Int)] -> Int
bagCount []          = 0
bagCount ((x,y):xs)  = y + bagCount xs

addToBag :: Char -> [(Char,Int)] -> [(Char,Int)]
addToBag ch []         =  [(ch,1)]
addToBag ch ((x,y):xs)
        | ch == x      =  (x,y+1) : xs
        | otherwise    = (x,y) : addToBag ch xs

removeFromBag :: Char -> [(Char,Int)] -> [(Char,Int)]
removeFromBag ch [] = []
removeFromBag ch ((x,y):xs)
       | ch == x && y == 1   = removeFromBag ch xs
       | ch == x             = (x,y-1) : xs
       | otherwise           = (x,y) : removeFromBag ch xs



    


       
        
       

      

                    












     


   
   

      
     
      
      
      
      