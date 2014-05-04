data Tree = Leaf [Int]
    | Node [(Tree, Int)] deriving (Show)

myTree :: Tree
myTree = Node
            [
                (Node
                    [
                        (Node
                            [(Leaf [3, 7, 8], 3)],
                        3),
                        (Node
                            [(Leaf [12, 23], 12)],
                        12),
                        (Node
                            [(Leaf [26], 3)],
                        26)
                    ],
                3),
                (Node
                    [
                        (Node
                            [(Leaf [32, 43, 98], 32)],
                        32),
                        (Node
                            [(Leaf [101], 101)],
                        101)
                    ],
                32)
            ]
main = print myTree
