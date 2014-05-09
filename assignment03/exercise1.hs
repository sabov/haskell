import Data.Ratio

-- Ex1

type Pupil = String -- the pupil's name
type Group = Int

count :: Int -> [Pupil] -> [(Group, Pupil)]
count numberOfGroups = zip xs where xs = countList 1 numberOfGroups

countList :: Int -> Int -> [Int]
countList from to = [from..to] ++ countList from to

-- Ex2

cantorList :: Int -> Int -> [Int]
cantorList sign to | sign > 0 = [1..to] ++ cantorList sign (to + 1)
                   | otherwise = reverse [1..to] ++ cantorList sign (to + 1)

--main = print $ count 3 ["Petro", "Frank", "Sandra", "Caro", "Max"]
main = print $ take 10 $ zip (cantorList 1 1) (cantorList (-1) 1)
