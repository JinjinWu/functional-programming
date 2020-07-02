module GameMasterDef where

-- This type represents winning or losing at the end of a game.
-- In the case of losing, the secret number is revealed.
data Ending = Lose Integer | Win deriving (Eq, Ord, Show)

-- Player's reply: either making a guess or surrendering.
data PlayerMsg = Guess Integer | Surrender
    deriving (Eq, Show)

-- The MonadGameMaster class contains the extra operation a game master needs.
class Monad m => MonadGameMaster m where
    -- This is how the game master communicates with the player.  The game
    -- master calls this function to send the lower and upper bounds to the
    -- player, then receives the player's message in the return value.
    gmAction :: Integer         -- Lower bound
             -> Integer         -- Upper bound
             -> m PlayerMsg

-- An example game master. This is meant to be goofy---it does not implement the
-- guessing game. But it shows how to code up a game master.
--
-- The goofy rules:
-- 0. Tell the player the initial range is from 1 to 100.
-- 1. If the player's first guess is an even number, instant win.
-- 2. Else, say that the range is now from 40 to 60.
-- 3. No matter what the player guesses now, they lose.  The "correct answer" is
--    42 unless they guessed 42, in which case it's 44 instead.
goofy :: MonadGameMaster m => m Ending
goofy = do
    req1 <- gmAction 1 100
    case req1 of
      Surrender -> return (Lose 42)
      Guess i
          | even i -> return Win
          | otherwise -> do
                req2 <- gmAction 40 60
                case req2 of
                  Surrender -> return (Lose 42)
                  Guess i
                    | i == 42 -> return (Lose 44)
                    | otherwise -> return (Lose 42)

-- You will make this type an instance of MonadGameMaster (and Monad etc).
data FreeGameMaster a
    = Pure a
    | GMAction Integer Integer (PlayerMsg -> FreeGameMaster a)

instance Show a => Show (FreeGameMaster a) where
    showsPrec d (Pure a) =
        showParen (d > 10) (showString "Pure " . showsPrec 11 a)
    showsPrec d (GMAction lo hi next) =
        showParen (d > 10) (showString "GMAction "
                            . shows lo . showChar ' ' . shows hi
                            . showString " (function)")
