import Data.Ratio

-- Ex1

type Pupil = String -- the pupil's name
type Group = Int

count :: Int -> [Pupil] -> [(Group, Pupil)]
count numberOfGroups = zip xs where xs = countList 1 numberOfGroups

countList :: Int -> Int -> [Int]
countList from to = [from..to] ++ countList from to

-- Ex2

cantorNumeratorList :: Int -> [Int]
cantorNumeratorList to = [1..to] ++ cantorNumeratorList (succ to)

cantorDenominatorList :: Int -> [Int]
cantorDenominatorList to = reverse [1..to] ++ cantorDenominatorList (succ to)

cantorList :: Int -> [Ratio Int]
cantorList index = if gcd a b > 1 then cantorList $ succ index
                                  else a % b : cantorList (succ index)
                                  where
                                      a = cantorNumeratorList 1 !! pred index
                                      b = cantorDenominatorList 1 !! pred index

cantor :: Int -> Ratio Int
cantor index = cantorList 1 !! pred index

--main = print $ count 3 ["Petro", "Frank", "Sandra", "Caro", "Max"]
main = print $ cantor 2 == 1 % 2
