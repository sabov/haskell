{-  
******************************************************************
Haskell function that checks whether all elements of one list are 
also contained in another list.
******************************************************************
Group Members 
1. Oleksandr Sabov   : 340311 : oleksandr.sabov@rwth-aachen.de
2. Petr Tarasenko    : 340826 : petr.tarasenko@rwth-aachen.de
3. Thomas Eck        : 317112 : thomas.eck1@rwth-aachen.de
4. Tanmaya Mahapatra : 340959 : tanmaya.mahapatra@rwth-aachen.de


eg. [0, -1] and [9, 0 , 3, -1]

-}

import Data.List

check :: [Int] -> [Int] -> Bool
check xs [] = False
check (x:xs) (y:ys) = if x == y then True
					  else check (x:xs) ys

extractInt :: [Int] -> [Int] -> Int
extractInt (x:xs) (y:ys) = if x == y then x
						else extractInt (x:xs) ys

containsAll :: [Int] -> [Int] -> Bool
containsAll [] [] = True
containsAll [] ys = True
containsAll xs [] = False
containsAll xs ys = if check xs ys == True then containsAll (delete (extractInt xs ys) xs) (delete (extractInt xs ys) ys)
							 else False

main = print $ containsAll [0, -1] [9, 0 , 3, -1]

{-  
[] [] = both lists are empty so return True
[] ys = The first list is empty, consequently all elements are contained.... reutrn True
xs [] = The second list is empty, consequently the remaining elements of xs cannot be contained in ys anymore... return False

Check....
True: The first element of xs was contained in ys based on our check function. Consequently we use extractInt to identify the Integer and delete to remvoe it from both lists. Then we call containsAll
again with the new lists

False: The first element of xs was not contained in ys. Consequently we can stop here and return False
-}
