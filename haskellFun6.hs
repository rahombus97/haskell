
-- possible pizza toppings
data Topping = Pepperoni | Onions | Ham | Mushrooms | Chicken
               deriving (Show)

-- sauces for pizzas and breadsticks
data Sauce = Tomato | Garlic | Pesto 
             deriving (Show)

-- sizes for pizzas and salads
data Size = Small | Large
            deriving (Show)

-- possible dressings for salads
data Dressing = Ranch | Greek | Caesar | None
                deriving (Show)

-- items on the menu for purchase
data MenuItem = Breadsticks Sauce
              | Salad Size Dressing
              | Pizza Size Sauce [Topping]
                deriving (Show)

breadsticks1, salad1, pizza1, pizza2 :: MenuItem
order1, order2 :: [MenuItem]

breadsticks1 = Breadsticks Pesto
salad1       = Salad Large Caesar
pizza1       = Pizza Small Garlic []
pizza2       = Pizza Large Tomato [Onions, Pepperoni, Pepperoni, Mushrooms]
order1       = [Salad Large None, Breadsticks Garlic, Pizza Large Tomato []]
order2       = [Pizza Small Tomato [Pepperoni, Mushrooms, Ham, Onions], Salad Small Ranch, Salad Large Greek, Pizza Large Pesto [Ham, Chicken], Breadsticks Garlic]

toppingCost :: Size -> Topping -> Float
toppingCost Small Chicken   = 2.50
toppingCost Large Chicken   = 3.25
toppingCost Small _         = 1.50
toppingCost Large _         = 2.25

sauceCost :: Size -> Sauce -> Float
sauceCost Small Pesto = 13.25
sauceCost Large Pesto = 16.50
sauceCost Small _     = 11.50
sauceCost Large _     = 14.00

cutCalories :: MenuItem -> MenuItem
cutCalories (Salad _ _)          = Salad Small None
cutCalories (Pizza _ s tpg)      = Pizza Small s tpg
cutCalories (Breadsticks s)      = Breadsticks s

willEat :: MenuItem -> Bool
willEat (Pizza _ Pesto _)      = False
willEat (Pizza _ _ xs)         = null [ xs | Mushrooms <- xs]
willEat (Breadsticks Pesto)    = False
willEat _ = True

price :: MenuItem -> Float
price (Pizza x y zs)      = sauceCost x y + sum [ toppingCost x a | a <- zs]
price (Salad Large _)     = 9.50
price (Salad Small _)     = 6.75
price (Breadsticks _)     = 4.25

numToppings :: [MenuItem] -> Int
numToppings xs = sum [ length zs | Pizza a b zs <- xs]

promotion :: [MenuItem] -> Float
promotion []    = 0
promotion [z]   = price z
promotion items = sum y - minimum y
     where
       y = [ price x | x <- items]






            

