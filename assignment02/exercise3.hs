data Genre = Nonfiction | Novel | Biography deriving (Eq, Show)
type Name = (String, String)
type Date = (Int, Int, Int) -- day, month, year
data Book = ABook Genre
                  Name   -- name of the author
                  String -- title of the book
                  Date   -- date of publication
                  Int    -- number of pages
            deriving Show

genre :: Book -> Genre
genre (ABook g _ _ _ _) = g
author :: Book -> Name
author (ABook _ a _ _ _) = a
title :: Book -> String
title (ABook _ _ t _ _) = t
date :: Book -> Date
date (ABook _ _ _ d _) = d
pages :: Book -> Int
pages (ABook _ _ _ _ p) = p
year :: Book -> Int
year b = let (_, _, y) = date b in y

breakingNews = ABook Novel ("Schaetzing", "Frank") "Breaking News" (06, 03, 2014) 976
snowden = ABook Nonfiction ("Harding", "Luke") "The Snowden Files" (06, 02, 2014) 346
futureShock = ABook Nonfiction ("Toffler", "Alvin") "Future Shock" (01, 06, 1984) 576

publishedIn :: Int -> [Book] -> [Book]
publishedIn year books = filter (\book -> case date book of
                                    (_, _, currYear) -> year == currYear
                                ) books

totalPages :: [Book] -> Int
totalPages books = foldr (\book acc -> pages book + acc) 0 books

toAuthor :: [Book] -> [Name]
toAuthor books = map (\book -> author book) books

titlesOf :: Genre -> [Book] -> [String]
titlesOf g books = map title . filter(\book -> g == genre book) $ books

main = print $ titlesOf Nonfiction [breakingNews, snowden, futureShock]
