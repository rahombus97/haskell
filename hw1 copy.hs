between :: Int -> Int -> Int -> Bool
between m y z = y<m && m<z

xor :: Bool -> Bool -> Bool
xor e1 e2 = (e1 || e2) && not (e1 && e2)

convertDtoF :: Float -> Float
convertDtoF temp = 212.0 - ((6/5)*temp)

convertFtoD :: Float -> Float
convertFtoD temp = (temp - 212.0)/(-(6/5))


