data IndexedTree a = Leaf [a] | Node [(IndexedTree a, a)] deriving (Show)

myTree :: IndexedTree Int
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
                32),
                (Node
                    [
                        (Leaf [132, 143, 198], 132),
                        (Leaf [1101], 1101)
                    ],
                132)
            ]

treeToList :: IndexedTree a -> [a]
treeToList (Leaf x) = x
treeToList (Node x) = foldl (\acc el -> case el of
                                (subTree, _) -> acc ++ treeToList subTree
                            ) [] x

cont :: Ord a => a -> IndexedTree a -> Bool
cont num (Leaf list) = elem num list
cont num (Node list) = cont num $ (\tree -> case tree of
                                        (subTree, num) -> subTree
                                    ) $ last $ filter (\node -> case node of
                                         (subTree, index) -> index <= num
                                    ) list

-- Added By Tanmaya
contains :: Ord a => a -> IndexedTree a -> Bool
contains e (Leaf xs) = elem e xs
contains e (Node x) = (\node ->  case node of
                                ((leaf1,index1):[]) -> contains e leaf1 
                                ((leaf1,index1):(leaf2,index2):[]) -> if e > index1 && e < index2 then contains e leaf1 else contains e (Node((leaf2, index2):[]))
                                ((leaf1,index1):(leaf2,index2):xss) -> if e > index1 && e < index2 then contains e leaf1 else contains e (Node((leaf2, index2):xss))
                      ) x


main = print $ cont 1102 myTree
