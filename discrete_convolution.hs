listProduct :: Num a => [a] -> [a] -> [a]
listProduct [] _ = []
listProduct _ [] = []
listProduct (a:as) (b:bs) = (a*b:listProduct as bs)

discreteConvStep :: Num a => [a] -> [a] -> Int -> a
discreteConvStep a b k = sum $ listProduct a $ positiveReflected $ b ++ cycle [0]
    where positiveReflected = reverse . take (k+1)

discreteConv :: Num a => [a] -> [a] -> [a]
discreteConv a b = map (discreteConvStep a b) [0..(length(a)+length(b)-2)]

main = do
    print $ discreteConv [1,2,3,4] [1,1,-1,1]

