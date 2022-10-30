module Main where

import Control.Monad (forever)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

import System.Random (randomRIO)

newtype WordList = WordList [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

misses :: Int
misses = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList $ lines dict


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ filter gameLength aw
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed remainingMisses) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) 
    ++ " Guessed so far: " ++ guessed ++ " Remaining misses: " ++ (show remainingMisses)
 
freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) [] misses

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) guess = guess `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses _) guess = guess `elem` guesses

remainingMisses :: Puzzle -> Int
remainingMisses (Puzzle _ _ _ m) = m

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x)  = x

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s missing) c matched = 
  Puzzle word newFilledInSoFar (c : s) (bool (missing - 1) missing matched)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling the word accordingly"
      return $ fillInCharacter puzzle guess True
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return $ fillInCharacter puzzle guess False

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed remainingMisses) =
  if remainingMisses == 0 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Gess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
