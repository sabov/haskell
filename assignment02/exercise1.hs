data Tree = Leaf [Int]
    | Node [(Tree, Int)] deriving (Show)

myTree :: Tree
myTree = Node
            [
                (Node
                    [
                        (Leaf [3, 7, 8], 3),
                        (Leaf [12, 23], 12),
                        (Leaf [26], 26)
                    ],
                3),
                (Node
                    [
                        (Leaf [32, 43, 98], 32),
                        (Leaf [101], 101)
                    ],
                32)
            ]
main = print myTree
