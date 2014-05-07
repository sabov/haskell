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

mapMult :: (a -> b) -> MultTree a -> MultTree b
mapMult fn (AMultTree num list) = AMultTree (fn num) (map (mapMult fn) list)

mean :: Int -> Int -> Int
mean x y = (x + y) `div` 2

depthFirstFold :: (b -> a -> b) -> b -> MultTree a -> b
depthFirstFold fn initValue (AMultTree num []) = fn initValue num
depthFirstFold fn initValue (AMultTree num list) = foldl (\acc subTree ->
                                                              depthFirstFold fn acc subTree
                                                          ) (fn initValue num) list


--main = print $ mapMult (\x -> even x) myTree
main = print $ depthFirstFold mean 1 myTree
