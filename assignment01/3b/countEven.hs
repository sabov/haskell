{-  
******************************************************************
Haskell function that counts the number of even elements of a list of integers.
******************************************************************
Group Members 
1. Oleksandr Sabov   : 340311 : oleksandr.sabov@rwth-aachen.de
2. Petr Tarasenko    : 340826 : petr.tarasenko@rwth-aachen.de
3. Thomas Eck        : 317112 : thomas.eck1@rwth-aachen.de
4. Tanmaya Mahapatra : 340959 : tanmaya.mahapatra@rwth-aachen.de
-}

getBool :: Int -> Bool
getBool x = if x == 0 then True else  False 
isEven :: Int -> Bool
isEven x = if x == 0 then getBool(x) else manP(x)
manP :: Int -> Bool
manP x = if x > 0 then decrNum(x) else incrNum(x)
decrNum :: Int -> Bool
decrNum x = if x > 0 then decrNum(x-2) else getBool(x)
incrNum :: Int -> Bool
incrNum x = if x < 0 then incrNum(x+2) else getBool(x)

countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs) = if isEven x then 1 + countEven xs  
					  else 0 + countEven xs
main = print $ isEven 5 




