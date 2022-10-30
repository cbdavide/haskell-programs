import Control.Monad
import Data.Char (toLower, isLetter)
import System.Exit (exitSuccess)

twoo :: IO Bool
twoo = do 
    c  <- getChar
    c' <- getChar
    return $ c == c'

sanitizeString :: String -> String
sanitizeString = map toLower . filter isLetter

checkIsPalindrome :: String -> Bool
checkIsPalindrome str = sanitized == reverse sanitized
  where sanitized = sanitizeString str

palindrome :: IO ()
palindrome = forever $ do
    line <- getLine
    if checkIsPalindrome line then
      putStrLn "It's a plindrome!"
    else do
        putStrLn "Nope!"
        exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ 
    "Name was: " ++ show name ++
    " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Input your name: "
  name <- getLine
  putStr "Input your age: "
  age <- getLine

  case mkPerson name $ read age of
    Right person ->
      putStrLn $ "Yay! Successfully got a person: " ++ show person
    Left NameEmpty ->
      putStrLn "Error: Empty name"
    Left AgeTooLow ->
      putStrLn "Error: Age should be gte to 0"
    Left (PersonInvalidUnknown errorMsg)->
      putStrLn $ "Error: " ++ errorMsg
