module GameMaster where

import Control.Monad (ap, liftM)
import Data.Maybe

import GameMasterDef

-- Question 1.

-- The game master for the guessing game.  The parameter is the secret number
-- the player has to guess.
guessingGame :: MonadGameMaster m => Integer -> m Ending
guessingGame secret
    | secret < 1 || secret > 100 = error "invalid game"
    | otherwise = gameLoop 1 100
        where
            gameLoop lower upper = do
                move <- gmAction lower upper
                case move of
                    Surrender -> return (Lose secret)
                    Guess i
                        | i == secret -> return Win
                        | i < lower || i > upper -> gameLoop lower upper
                        | i < secret -> gameLoop (i + 1) upper
                        | i > secret -> gameLoop lower (i - 1)


-- Question 2.

instance Functor FreeGameMaster where
    -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    fmap = liftM

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- If you are confident with your Monad instance, you can just write
    pure = return

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a
    return = Pure

    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    (Pure a) >>= k = k a
    (GMAction lower upper f) >>= k = GMAction lower upper (\msg -> f msg >>= k)

instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    gmAction lower upper = GMAction lower upper return


-- Question 3.

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame gm = foldl (\acc i -> isJust (test i) && acc) True [1..100]
    where
        test secret = do
            -- 1st gmAction call
            f1 <- areBoundsCorrect 1 100 (gm secret)
            checkWinLose f1
            -- out of bounds
            f2 <- areBoundsCorrect 1 100 (f1 (Guess (secret+150)))
            checkWinLose f2
            f3 <- areBoundsCorrect 1 100 (f1 (Guess (secret-150)))
            checkWinLose f3
            -- less than secret guess
            foldl (\acc j -> if (isJust (testAllLess f2 (getLess j)) && isJust acc) then Just () else Nothing) (Just ()) [1..secret-1]
            -- more than secret guess
            foldl (\acc j -> if (isJust (testAllMore f2 (getMore j)) && isJust acc) then Just () else Nothing) (Just ()) [secret+1..100]
            -- check 2 wrong guesses in a row within range with all possible ranges and if bounds stay the same with out of bounds guess
            foldl (\acc (j,k) -> if (isJust (testLessMore f2 (getLess j) (getMore k)) && isJust acc) then Just () else Nothing) (Just ()) allPossibleRanges
                where
                    checkWinLose f = do
                        isWin (f (Guess secret))
                        isLose secret (f Surrender)
                    getLess j
                        | secret == 1 = 0
                        | otherwise = max (secret-j) 1
                    getMore j
                        | secret == 100 = 101
                        | otherwise = min (secret+j) 100
                    testAllLess g1 j = do
                        g2 <- areBoundsCorrect (j+1) 100 (g1 (Guess j))
                        checkWinLose g2
                    testAllMore g1 j = do
                        g2 <- areBoundsCorrect 1 (j-1) (g1 (Guess j))
                        checkWinLose g2
                    allPossibleRanges = [(j,k) | j <- [1..secret-1], k <- [secret+1..100]]
                    testLessMore g1 j k = do
                        g2 <- areBoundsCorrect (j+1) 100 (g1 (Guess j))
                        checkWinLose g2
                        g3 <- areBoundsCorrect (j+1) (k-1) (g2 (Guess k))
                        checkWinLose g3
                        -- the bounds are unchanged here
                        g4 <- areBoundsCorrect (j+1) (k-1) (g3 (Guess (k+50)))
                        checkWinLose g4


isWin :: FreeGameMaster Ending -> Maybe ()
isWin (Pure Win) = Just ()
isWin _ = Nothing

isLose :: Integer -> FreeGameMaster Ending -> Maybe ()
isLose secret (Pure (Lose n))
    | secret == n = Just ()
    | otherwise = Nothing
isLose _ _ = Nothing

areBoundsCorrect :: Integer -> Integer -> FreeGameMaster Ending -> Maybe (PlayerMsg -> FreeGameMaster Ending)
areBoundsCorrect lower upper (GMAction l u f)
    | lower == l && upper == u = Just f
    | otherwise = Nothing
areBoundsCorrect _ _ _ = Nothing
