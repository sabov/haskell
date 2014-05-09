type Pupil = String -- the pupil's name
type Group = Int

count :: Int -> [Pupil] -> [(Group, Pupil)]
count numberOfGroups = zip xs where xs = countList 1 numberOfGroups

countList :: Int -> Int -> [Int]
countList from to = [from..to] ++ countList from to

main = print $ count 3 ["Petro", "Frank", "Sandra", "Caro", "Max"]
