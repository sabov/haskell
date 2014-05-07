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

depthFirstFold :: (b -> a -> b) -> b -> MultTree a -> b
depthFirstFold fn initValue (AMultTree num []) = fn initValue num
depthFirstFold fn initValue (AMultTree num list) = foldl (\acc subTree ->
                                                              depthFirstFold fn acc subTree
                                                          ) (fn initValue num) list


--Call to check the expected result
--main = print $ mean (mean (mean (mean (mean (mean (mean 0 8) 3) (-56)) 4) 987) 4 ) 6

--Live call of the programm
main = print $ depthFirstFold mean 0 myTree
