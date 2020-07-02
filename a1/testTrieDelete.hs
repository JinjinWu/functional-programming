{-
How to use:

* All tests: runghc testTrieDelete.hs

* Individual test e.g. 2nd: runghc testTrieDelete.hs 1
-}

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Test.HUnit
import           Text.Read (readMaybe)

import           TrieDef
import           TrieDelete (trieDelete)

-- Re-assert most general type.
td :: [Char] -> Trie a -> Trie a
td = trieDelete

tests = [ td "" albertTrie ~?= t0
        , td "pi" albertTrie ~?= t1
        , td "ace" albertTrie ~?= t2
        , td "foo" albertTrie ~?= albertTrie
        -- more tests when marking
        ]

t0, t1, t2 :: Trie Integer

t0 = TrieNode Nothing (Map.fromList [('a', a), ('p', p), ('t', t)])
  where
    a = TrieNode Nothing (Map.fromList [('c', ac)])
    ac = TrieNode Nothing (Map.fromList [('e', ace)])
    ace = TrieNode (Just 9) Map.empty
    p = TrieNode Nothing (Map.fromList [('i', pi)])
    pi = TrieNode (Just 1) (Map.fromList [('t', pit)])
    pit = TrieNode (Just 9) Map.empty
    t = TrieNode Nothing (Map.fromList [('o', to)])
    to = TrieNode Nothing (Map.fromList [('p', top), ('n', ton)])
    top = TrieNode (Just 5) Map.empty
    ton = TrieNode (Just 7) Map.empty

t1 = TrieNode (Just 4) (Map.fromList [('a', a), ('p', p), ('t', t)])
  where
    a = TrieNode Nothing (Map.fromList [('c', ac)])
    ac = TrieNode Nothing (Map.fromList [('e', ace)])
    ace = TrieNode (Just 9) Map.empty
    p = TrieNode Nothing (Map.fromList [('i', pi)])
    pi = TrieNode Nothing (Map.fromList [('t', pit)])
    pit = TrieNode (Just 9) Map.empty
    t = TrieNode Nothing (Map.fromList [('o', to)])
    to = TrieNode Nothing (Map.fromList [('p', top), ('n', ton)])
    top = TrieNode (Just 5) Map.empty
    ton = TrieNode (Just 7) Map.empty

t2 = TrieNode (Just 4) (Map.fromList [('p', p), ('t', t)])
  where
    p = TrieNode Nothing (Map.fromList [('i', pi)])
    pi = TrieNode (Just 1) (Map.fromList [('t', pit)])
    pit = TrieNode (Just 9) Map.empty
    t = TrieNode Nothing (Map.fromList [('o', to)])
    to = TrieNode Nothing (Map.fromList [('p', top), ('n', ton)])
    top = TrieNode (Just 5) Map.empty
    ton = TrieNode (Just 7) Map.empty

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
