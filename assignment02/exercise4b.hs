data MultTree a = AMultTree a [MultTree a] deriving Show

myTree :: MultTree Int
myTree = AMultTree 8 [
        AMultTree 3 [
            AMultTree (-56) [],
            AMultTree 4 [],
            AMultTree 987 []
        ],
        AMultTree 4 [AMultTree 6 []]
    ]

mean :: Int -> Int -> Int
mean x y = (x + y) `div` 2
    
getElements :: MultTree Int -> [Int]
getElements (AMultTree num []) = [num]
getElements (AMultTree num list) = (\acc el -> case el of
                                (x:[]) -> num:acc ++ getElements x 
				(x:xs:[]) -> num:acc ++ getElements x ++ getElements xs 
				(x:xs:xss) -> num:acc ++ getElements x ++ getElements xs ++ (\(x:_) -> acc ++ getElements x) xss
                            ) [] list

depthFirstFold :: (Int -> Int -> int) -> Int -> MultTree Int -> Int
depthFirstFold fn x y = foldl mean x (getElements y)
    
    
--Call to check if the computed List is OK
--main = print $ getElements myTree

--Call to check the expected result
--main = print $ mean (mean (mean (mean (mean (mean (mean 0 8) 3) (-56)) 4) 987) 4 ) 6

--Live call of the programm
main = print $ depthFirstFold mean 0 myTree
