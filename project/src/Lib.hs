module Lib
 ( Color(..)
 , GameMode(..)
 , GameState(..)
 , initializeGame
 , makeGuess
 , validateGuess
 ) where 

import System.Random
import System.IO

data Color = Grey | Yellow | Green deriving (Eq, Show)

data GameMode = Easy | Expert deriving (Eq, Show)

data GameState = GameState
  { word :: String
  , guessedWord :: String
  , guessedColors :: [Color]
  , gameMode :: GameMode
  , previousGuesses :: [(String, [Color])]
  }


------------------------------------------------------------------------------
initializeGame :: GameMode -> IO GameState
initializeGame mode = do
  -- Read the list of words from the file and choose a random word
  wordList <- readWordListFromFile "wordlist.txt"
  chosenWord <- chooseRandomWord wordList

  -- Initialize the game state
  let initialGameState = GameState
        { word = chosenWord
        , guessedWord = replicate (length chosenWord) '_'
        , guessedColors = replicate (length chosenWord) Grey
        , gameMode = mode
        , previousGuesses = []
        }

  return initialGameState
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Validate the guess and update the game state
makeGuess :: GameState -> String -> IO GameState
makeGuess gameState guess = do
  let validatedGameState = validateGuess gameState guess
  let updatedGameState = updateGameState validatedGameState guess
  return updatedGameState
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Update the game state with the new guess and colors
updateGameState :: GameState -> String -> GameState
updateGameState gameState guess =
  let newColors = getColors (word gameState) guess
      updatedGuesses = (guess, newColors) : previousGuesses gameState
      updatedGuessedWord = updateGuessedWord (word gameState) guess (guessedWord gameState)
  in gameState
    { guessedWord = updatedGuessedWord
    , guessedColors = newColors
    , previousGuesses = updatedGuesses
    }

updateGuessedWord :: String -> String -> String -> String
updateGuessedWord _ [] pGuessedWord = pGuessedWord
updateGuessedWord pWord (g:guess) (gw:pGuessedWord)
  | g == head pWord = g : updateGuessedWord (tail pWord) guess pGuessedWord
  | otherwise = gw : updateGuessedWord (tail pWord) guess pGuessedWord
updateGuessedWord _ _ _ = "Something went wrong in function updateGuessedWord!"

getColors :: String -> String -> [Color]
getColors pWord guess = zipWith determineColor pWord guess
  where
    determineColor :: Char -> Char -> Color
    determineColor w g
      | w == g = Green
      | g `elem` pWord = Yellow
      | otherwise = Grey
------------------------------------------------------------------------------

------------------------------------------------------------------------------
validateGuess :: GameState -> String -> GameState
validateGuess gameState guess =
  let validGuess = isValidGuess (gameMode gameState) gameState guess
  in if validGuess
     then gameState
     else gameState { guessedColors = replicate (length guess) Grey }

isValidGuess :: GameMode -> GameState -> String -> Bool
isValidGuess mode gameState guess =
  case mode of
    Easy -> isValidEasyGuess gameState guess
    Expert -> isValidExpertGuess gameState guess

isValidEasyGuess :: GameState -> String -> Bool
isValidEasyGuess gameState guess =
  let prevGuesses = previousGuesses gameState
  in all (not . contradictsPreviousGuess gameState guess) prevGuesses

isValidExpertGuess :: GameState -> String -> Bool
isValidExpertGuess gameState guess =
  let prevGuesses = previousGuesses gameState
      numLies = length (filter (\(_, colors) -> containsIncorrectColors colors) prevGuesses)
  in numLies < 1 && not (contradictsPreviousGuess gameState guess (head prevGuesses))

contradictsPreviousGuess :: GameState -> String -> (String, [Color]) -> Bool
contradictsPreviousGuess gameState guess (prevGuess, prevColors) =
  let newColors = getColors (word gameState) guess
      charNewColors = map colorToChar newColors
  in length prevGuess /= length guess ||
     any (\(c1, c2) -> c1 == Grey && c2 /= Grey) (zip prevColors newColors) ||
     any (\(c1, c2) -> c1 == Yellow && c2 == Grey) (zip prevColors newColors) ||
     any (\(c1, c2) -> c1 == Green && c2 /= head prevGuess) (zip prevColors (take (length prevGuess) charNewColors))

colorToChar :: Color -> Char
colorToChar Grey = '-'
colorToChar Yellow = 'x'
colorToChar Green = 'o'

containsIncorrectColors :: [Color] -> Bool
containsIncorrectColors colors = any (\c -> c /= Green && c /= Grey) colors
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Helper functions for file IO, random word selection, and game state updates
readWordListFromFile :: FilePath -> IO [String]
readWordListFromFile filePath = do
  handle <- openFile filePath ReadMode
  wordList <- readWords handle
  hClose handle
  return wordList

readWords :: Handle -> IO [String]
readWords handle = do
  eof <- hIsEOF handle
  if eof
    then return []
    else do
      readWord <- hGetLine handle
      remainingWords <- readWords handle
      return (readWord : remainingWords)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
chooseRandomWord :: [String] -> IO String
chooseRandomWord wordList = do
  index <- randomRIO (0, length wordList - 1)
  return (wordList !! index)
------------------------------------------------------------------------------
