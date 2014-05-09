{-  
******************************************************************
Haskell function that computes the Pefix sum of a given Int List.
******************************************************************
Group Members 
1. Oleksandr Sabov   : 340311 : oleksandr.sabov@rwth-aachen.de
2. Petr Tarasenko    : 340826 : petr.tarasenko@rwth-aachen.de
3. Thomas Eck        : 317112 : thomas.eck1@rwth-aachen.de
4. Tanmaya Mahapatra : 340959 : tanmaya.mahapatra@rwth-aachen.de
-}


--Funtion to check if a List is Empty or not
isnull :: [Int] -> Bool
isnull [] = True
isnull xs = False

--Function to compute the Length of a List of Integers
summ :: [Int] -> Int
summ []     = 0
summ (x:xs) = x + summ xs

--Function to return the list after deleting the last element
del_last :: [Int] -> [Int]
del_last []  = []
del_last (x:[]) = []
del_last (x:xs:[]) = [x]
del_last (x:xs) = x: del_last xs

--Function : Core Functionality
perform_prefixsum :: [Int]  ->[Int] -> [Int]
perform_prefixsum xs ys  = if (isnull xs)
	              then  ys
		      else do			
        	       perform_prefixsum  (del_last xs) ((summ xs):ys)

prefixsum :: [Int] -> [Int]
prefixsum xs = perform_prefixsum xs []
 
main = print $ prefixsum [5,1,6,1]
