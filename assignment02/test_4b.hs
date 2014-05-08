
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
mapMult func (AMultTree a []) = AMultTree (func a ) []
mapMult func (AMultTree a x) = AMultTree (func a ) (map (mapMult func) x)

depthFirstFoldhelp :: (b -> a -> b) -> b -> [MultTree a] -> b
depthFirstFoldhelp _ e [] = e
depthFirstFoldhelp func e (x:xs) = depthFirstFoldhelp func (depthFirstFold func e x) xs
depthFirstFold :: (b -> a -> b) -> b -> MultTree a -> b
depthFirstFold func e (AMultTree a []) = func e a
depthFirstFold func e (AMultTree a l) = depthFirstFoldhelp func (func e a) l
mean :: Int -> Int -> Int
mean x y = (x+y) `div` 2

main = print $ depthFirstFold mean 0 myTree