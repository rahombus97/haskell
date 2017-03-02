import Data.Char

duplicates :: String -> Bool
duplicates []      = False
duplicates (x:xs)  = elem x xs || duplicates xs
    
zap ch (c:cs) = [ x | x <- (c:cs), ch /= x  ] 

unique :: String -> String
unique [] = []
unique (c:cs) 
       | elem c cs  = zap c (unique cs)
       | otherwise  = c : unique cs

prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) 
       | x == y     = prefix xs ys
       | otherwise  = False

subseq :: String -> String -> Bool
subseq [] _ = True
subseq _ [] = False
subseq (x:xs) (y:ys)
       | x == y     = subseq xs ys
       | otherwise  = subseq (x:xs) ys

substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring (x:xs) (y:ys)
          | x == y     = prefix xs ys
          | otherwise  = substring (x:xs) ys

subsequences :: String -> [String]
subsequences []      = [[]]
subsequences (x:xs)  = [ x:rest | rest <- subsequences xs] ++ subsequences xs
      



      

                
    
       
       
    
 
   

    
       

       
       

   
