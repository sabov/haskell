import System.Directory
import Data.Char
import Data.List

main :: IO ()
main = do
  recipes <- findRecipes
  if (null recipes) then putStrLn "No recipes available..."
  else do
    promptForInput recipes
    meal <- readInput recipes
    if (isExitCommand meal) then putStrLn "Leaving, good bye..."
    else do
      showRecipe meal
      main

findRecipes :: IO [String]
findRecipes = do
  files <- getDirectoryContents "."
  return (map removeEnding (filter (isSuffixOf ending) files))

ending :: String
ending = ".recipe"

removeEnding :: String -> String
removeEnding str = takeWhile (/='.') str

-- list all available meals and ask the user to input the name of the
-- desired meal
promptForInput :: [String] -> IO ()
promptForInput recipes = do
    putStr "Available recipes: "
    putStrLn $ intercalate ", " recipes
    putStrLn "What do you want to cook?"

-- reads the input from the user until he either inputs the name of an
-- existing meal or an exit command (see isExitCommand)
readInput :: [String] -> IO String
readInput recipes = do
  meal <- getLine
  if (not (meal `elem` recipes) && not (isExitCommand meal))
  then do
    putStrLn "I do not know a recipe for this meal, try again..."
    promptForInput recipes
    readInput recipes
  else
    return meal

isExitCommand :: String -> Bool
isExitCommand cmd = (map toLower cmd) `elem` ["quit", "exit", "q", "e"]

-- print the recipe with the given name on the command line
showRecipe :: String -> IO ()
showRecipe meal = do
    file <- readFile $ meal ++ ending
    putStrLn "==========================================="
    putStrLn file
    putStrLn "==========================================="
