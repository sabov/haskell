:maindata IndexedTree a = Leaf [a] | Node [(IndexedTree a, a)] deriving (Show)

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
                32)
            ]

treeToList :: IndexedTree a -> [a]
treeToList (Leaf x) = x
treeToList (Node x) = foldl (\acc el -> case el of
                                (subTree, _) -> acc ++ treeToList subTree
                            ) [] x

contains :: Ord a => a -> IndexedTree a -> Bool
contains num (Leaf list) = elem num list
contains num (Node list) = contains num $ (\tree -> case tree of
                                        (subTree, num) -> subTree
                                    ) $ last $ filter (\node -> case node of
                                         (subTree, index) -> index <= num
                                    ) list

--main = print $ treeToList myTree
main = print $ contains 22 myTree
