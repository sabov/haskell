{-  
******************************************************************
Haskell Infix Operator
******************************************************************
Group Members 
1. Oleksandr Sabov   : 340311 : oleksandr.sabov@rwth-aachen.de
2. Petr Tarasenko    : 340826 : petr.tarasenko@rwth-aachen.de
3. Thomas Eck        : 317112 : 
4. Tanmaya Mahapatra : 340959 : tanmaya.mahapatra@rwth-aachen.de
-}

--Function to return the First Element of a List

firstelement:: [Int] -> Int
firstelement [] = 0
firstelement (x:xs) = x

--Funtion to check if a List is Empty or not
isnull :: [Int] -> Bool
isnull [] = True
isnull xs = False

--Function to remove all occurences of n grom the given list []
removeall :: Int -> [Int] -> [Int]
removeall n []=[]
removeall n [x]|n==x=[] 
               |otherwise=[x]
removeall n (x:xs)|n==x=removeall n xs 
                  |otherwise=x:removeall n xs

--Function to reverse a List of Integers
swap :: [Int] -> [Int] -> [Int]
swap xs ys = if (null xs)
		     then ys
		     else do
		      swap (removeall (firstelement xs ) xs)  ((firstelement xs) : ys)

--Core Functionality 
iterate_concatenate :: [Int] -> [Int] -> [Int] -> [Int]
iterate_concatenate xs ys zs = if (isnull (xs ++ ys))
	              then  swap zs []
		      else do
	               iterate_concatenate (removeall (firstelement (xs ++ ys)) xs) (removeall (firstelement (xs ++ ys)) ys) ((firstelement (xs ++ ys)) : zs)

(+|+) :: [Int] -> [Int] -> [Int]
(+|+) xs ys = iterate_concatenate xs ys []

main = print $ firstelement [1,2]


