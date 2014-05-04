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
                32)
            ]

treeToList :: IndexedTree a -> [a]
treeToList (Leaf x) = x
treeToList (Node x) = foldl (\acc el -> case el of
                                (subTree, _) -> acc ++ treeToList subTree
                            ) [] x

contains :: Ord a => a -> IndexedTree a -> Bool
contains num (Leaf x) = elem num x
contains num (Node x) = forEachTwoElements (\a b -> case a of
                                                (subTree1, index1) -> case b of
                                                    (subTree2, index2) -> if num >= index1 && num < index2
                                                                then contains num subTree1
                                                                else False
                                           ) x


forEachTwoElements :: (a -> a -> b) -> [a] -> [b]
forEachTwoElements fn [] = []
forEachTwoElements fn (x:[]) = []
forEachTwoElements fn (x:xs:xss) = fn x xs : forEachTwoElements fn (xs:xss)

main = print $ contains 22 myTree
