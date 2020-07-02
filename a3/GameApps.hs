{-# LANGUAGE RankNTypes #-}
module GameApps where

import Text.Read

import GameMasterDef 
import qualified GameMaster as T

-- Make IO an instance that communicates with the player through stdio.
instance MonadGameMaster IO where
    gmAction lo hi = askPlayer lo hi

-- Given the lower and upper bounds, ask the player for their guess or surrender.
-- (If invalid input, ask again.)
-- Return the PlayerMsg representation of their reply.
-- This function shared by both the IO instance above and guessFreeIO below.
askPlayer :: Integer -> Integer -> IO PlayerMsg
askPlayer lo hi = inputLoop
  where
    inputLoop = do
        putStrLn ("The number is between " ++ show lo ++ " and " ++ show hi ++ ".")
        putStrLn "Please enter your guess or enter \"surrender\"."
        inp <- getLine
        if inp == "surrender"
            then return Surrender
            else case readMaybe inp of
                   Nothing -> do
                       putStrLn "Sorry, that's not a number."
                       inputLoop
                   Just g -> return (Guess g)

-- Print nicely the final outcome of a game.
printOutcome :: Ending -> IO ()
printOutcome (Lose answer) = putStrLn ("You have lost :(  The answer is " ++ show answer)
printOutcome Win = putStrLn "You have won :)"

-- Run a given game using the IO instance of MonadGameMaster.
-- (E.g., try "playIO goofy".)
playIO :: (forall m. MonadGameMaster m => m Ending) -> IO ()
playIO game = do
    outcome <- game
    printOutcome outcome

-- Create and run the guessing game using the IO instance of MonadGameMaster.
-- The parameter is the secret number the player needs to guess.
guessIO :: Integer -> IO ()
guessIO n = playIO (T.guessingGame n)

-- Run a given game using the FreeGameMaster instance of MonadGameMaster, still
-- using stdio.
playFreeIO :: (forall m. MonadGameMaster m => m Ending) -> IO ()
playFreeIO game = go game
  where
    go :: FreeGameMaster Ending -> IO ()
    go (Pure outcome) = printOutcome outcome
    go (GMAction lo hi next) = do
        req <- askPlayer lo hi
        go (next req)

-- Create and run the guessing game using the FreeGameMaster instance of
-- MonadGameMaster, still using stdio.  The parameter is the secret number the
-- player needs to guess.
guessFreeIO :: Integer -> IO ()
guessFreeIO n = playFreeIO (T.guessingGame n)
