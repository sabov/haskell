data Regex = Symbol Char
            | Empty
            | Concat Regex Regex
            | Or Regex Regex
            | Kleene Regex
	    	

