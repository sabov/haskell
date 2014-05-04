data Regex =  Empty
            | Symbol Char
	    | Kleene Regex
            | Concat Regex Regex
            | Or Regex Regex
            

instance Show Regex where
	show Empty = "{}"
	show (Symbol s) = [s]
	show (Kleene star) = "(" ++ show star ++ ")" ++ "*"
	show (Concat l r) = show l ++ show r
	show (Or l r) = "(" ++ show l ++ " | " ++ show r ++ ")" 

