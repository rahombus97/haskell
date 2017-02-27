import Data.Char

compareChars :: Char -> Char -> Char -> String
compareChars a b c 
     | a == b && a == c && b == c = "All equal"
     | a == b || a == c || b == c = "Two match" 
     | otherwise = "All distinct"

combine :: Int -> Int -> Int -> Int
combine x y z  
    | x <=9 && x >=0 && y <=9 && y >=0 && z <=9 && z >=0 = 100*x + 10*y + 1*z
    | otherwise = -1    

splitFloat :: Float -> (Integer, Float)
splitFloat num  
           | num >= 0 = (floor num, num - fromInteger(floor num)) 
           | num <= 0 = (ceiling num, fromInteger(ceiling num) + (-1) * num)  

stdCost :: Int -> Int -> Bool -> Int
stdCost dev gb new 
            | dev < 1 || gb < 0 = -1  
            | gb <=4 && new == True = 50 + 10*(dev-1) - 25
            | gb <=4 && new == False = 50 + 10*(dev-1)                          
            | new == True = (50 + 10*(dev-1) + (gb-4)*15) - 25
            | new == False = (50 + 10*(dev-1) + (gb-4)*15) 

powerCost :: Int -> Int -> Bool -> Int
powerCost dev gb new
          | dev < 1 || gb < 0 = -1                      
          | gb <=10+(2*dev) && new == True = 80 + 8*(dev-1) - 25
          | gb <=10+(2*dev) && new == False = 80 + 8*(dev-1)   
          | new == True = 80 + 8*(dev-1) + ((gb-(10+(2*dev)))*20) - 25
          | new == False = 80 + 8*(dev-1) + ((gb-(10+(2*dev)))*20)

bestPlan :: Int -> Int -> String
bestPlan dev gb
         | (powerCost dev gb False) > (stdCost dev gb False) = "Standard"
         | (stdCost dev gb False) > (powerCost dev gb False) = "Power User"                                                      