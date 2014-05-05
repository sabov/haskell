data MultTree a = AMultTree a [MultTree a] deriving Show

myTree :: MultTree Int
myTree =  AMultTree 8 [
        AMultTree 3 [
            AMultTree (-56) [],
            AMultTree 4 [],
            AMultTree 987 []
        ],
        AMultTree 4 [AMultTree 6 []]
    ]

mapMult :: (a -> b) -> MultTree a -> MultTree b
mapMult fn (AMultTree num list) = AMultTree (fn num) (map mapMult list)

main = print $ mapMult (\x -> x * x) myTree
