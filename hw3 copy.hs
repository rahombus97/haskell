import Data.Char

squarePairs :: Int -> Integer -> [(Integer,Integer)]
squarePairs n i 
         | n <= 0    = []
         | otherwise = (i,i^2) : squarePairs (n-1) (i+1)

                          
countDownBy :: Int -> Int -> Int -> [Int]
countDownBy m n diff
            | m < n     = []
            | otherwise = m : countDownBy (m-diff) n diff

steps :: Int -> Int -> [[Int]]
steps m n
    | n <= 0      = []
    | otherwise   = helper 1
   where
     helper :: Int -> [[Int]]
     helper i
            | i > (n-m)+1  = []
            | otherwise    = countUp m ((m+i)-1) : helper (i+1)

countUp :: Int -> Int -> [Int]
countUp m n 
    | m > n     = []
    | otherwise = m : countUp (m+1) n

indexChar :: Int -> Int -> Char -> String
indexChar n i c  
    | n <= 0    = []
    | otherwise = helper 1
   where
     helper :: Int -> String
     helper y          
          | y > n || i < 1      = []               
          | y == i              = '!' : helper (y+1)                 
          | otherwise           = c : helper (y+1)

diag :: Int -> Char -> [String]
diag n c 
     | n <= 0  = []
     | otherwise = helper 1
   where
     helper :: Int -> [String]
     helper i 
       | i > n   = []
       | otherwise = indexChar n i c : helper (i+1)

