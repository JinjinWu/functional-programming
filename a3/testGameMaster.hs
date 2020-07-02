-- How to use: runghc testGameMaster.hs

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Test.HUnit
import           Text.Read (readMaybe)

import           Control.Monad (ap, liftM)
import           GHC.Stack (HasCallStack)

import qualified GameMaster as C
import           GameMasterDef

-- Re-assert desired types

guessingGame :: MonadGameMaster m => Integer -> m Ending
guessingGame = C.guessingGame

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame = C.testGame


------------------------------------
-- Setup code for testing question 1
------------------------------------

-- This instance plays by a binary search strategy (and so should always win).
-- Internally it just has to store the "return value" for return/pure.  The
-- binary search strategy is implemented in gmAction.
newtype BS a = MkBS a
runBS (MkBS a) = a
instance Monad BS where
    return = MkBS
    MkBS a >>= k = k a
instance Functor BS where
    fmap = liftM
instance Applicative BS where
    pure = return
    (<*>) = ap
instance MonadGameMaster BS where
    gmAction lo hi = return (Guess (div (hi + lo) 2))

-- This instance always surrenders immediately. (SF = sad face)
newtype SF a = MkSF a
runSF (MkSF a) = a
instance Monad SF where
    return = MkSF
    MkSF a >>= k = k a
instance Functor SF where
    fmap = liftM
instance Applicative SF where
    pure = return
    (<*>) = ap
instance MonadGameMaster SF where
    gmAction lo hi = return Surrender

testsQ1 =
    [ "Q1: binary search strategy" ~: runBS (guessingGame 42) ~?= Win
    , "Q1: sad face strategy" ~: runSF (guessingGame 42) ~?= Lose 42
      -- more tests when marking
    ]


------------------------------------
-- Setup code for testing question 2
------------------------------------

-- These are just very dumb programs written in FreeGameMaster for testing fmap,
-- pure, <*>, and >>=.  They do not really do meaningful or useful things.

-- Send the range 5...50. No matter what the player replies, return the length
-- function.
foo1 :: FreeGameMaster (String -> Int)
foo1 = GMAction 5 50 (\_ -> Pure length)

-- Send the range 10...30. If the player surrenders, return the string "give
-- up"; if the player guesses i, return a string of i 'x's.
foo2 :: FreeGameMaster String
foo2 = GMAction 10 30 (\req -> case req of
                          Surrender -> Pure "give up"
                          Guess i -> Pure (replicate (fromIntegral i) 'x'))

-- First do what foo2 does.  Let n be the length of the returned string.
-- Now tell the player the new range is n...n+10.  No matter what the
-- player does next, return ().
foo3 :: FreeGameMaster ()
foo3 = foo2 >>= \str -> let n = fromIntegral (length str)
                        in GMAction n (n+10) (\_ -> Pure ())

foo4 :: FreeGameMaster PlayerMsg
foo4 = gmAction 47 59

-- Compare the behaviour of a game master against expected steps, using
-- pre-recorded player messages.
bisimulates :: (HasCallStack, Eq a, Show a)
            => [PlayerMsg]        -- pre-recorded player messages
            -> FreeGameMaster a   -- the game master to be tested
            -> FreeGameMaster a   -- expected steps
            -> Test
bisimulates ps prog expected = TestCase (go ps prog expected 1)
  where
    go _ (Pure a) (Pure ex) n =
        assertEqual ("In step " ++ show n) ex a
    go _ gm@(Pure _) ex@(GMAction _ _ _) n =
        assertFailure ("In step " ++ show n
                       ++ "\nexpected: " ++ show ex
                       ++ "\n but got: " ++ show gm)
    go (p:ps) gm@(GMAction lo1 hi1 next1) ex@(GMAction lo2 hi2 next2) n
        | lo1 == lo2, hi1 == hi2 = go ps (next1 p) (next2 p) (n+1)
        | otherwise = assertFailure ("In step " ++ show n
                                     ++ "\nexpected: "
                                     ++ show ex
                                     ++ "\n but got: "
                                     ++ show gm)
    go _ gm@(GMAction _ _ _) ex@(Pure _) n =
        assertFailure ("In step " ++ show n
                       ++ "\nexpected: " ++ show ex
                       ++ "\n but got: " ++ show gm)

testsQ2 = [ "Q2: fmap"
            ~: [ "surrender case"
                 ~: bisimulates
                      [Surrender]
                      (fmap length foo2)
                      (GMAction 10 30 (\_ -> Pure 7))
               , "guess case"
                 ~: bisimulates
                      [Guess 42]
                      (fmap length foo2)
                      (GMAction 10 30 (\_ -> Pure 42))
               ]
          , "Q2: (<*>)"
            ~: bisimulates
                 [Guess 4, Guess 42]
                 (foo1 <*> foo2)
                 (GMAction 5 50 (\_ -> GMAction 10 30 (\_ -> Pure 42)))
          , "Q2: (>>=)"
            ~: bisimulates
                 [Guess 10, Guess 20]
                 foo3
                 (GMAction 10 30 (\_ -> GMAction 10 20 (\_ -> Pure ())))
          , "Q2: gmAction"
            ~: [ "guess case"
                 ~: bisimulates
                      [Guess 50]
                      foo4
                      (GMAction 47 59 (\_ -> Pure (Guess 50)))
               , "surrender case"
                 ~: bisimulates
                      [Surrender]
                      foo4
                      (GMAction 47 59 (\_ -> Pure Surrender))
               ]
            -- more tests when marking
          ]
-- Beware: My expected steps are tailor-made for the pre-recorded player
-- messages I give.  E.g., "fmap length foo2" is NOT meant to be identical to
-- "GMAction 10 30 (\_ -> Pure 42)" in general; rather, they just happen to
-- behave the same if the player says "Guess 42".


------------------------------------
-- Setup code for testing question 3
------------------------------------

testsQ3 = [ "Q3: goofy" ~: testGame (\_ -> goofy) ~?= False
            -- more tests when marking
          ]


-----------------------------
-- Putting all tests together
-----------------------------

tests = testsQ1 ++ testsQ2 ++ testsQ3

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)
