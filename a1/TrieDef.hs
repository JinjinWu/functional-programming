module TrieDef where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.PrettyPrint as Doc hiding ((<>))

data Trie a = TrieNode (Maybe a) (Map Char (Trie a))
    deriving (Eq, Show)

prettyTrie :: Show a => Trie a -> String
prettyTrie trie = (render . prettyRoot) trie
  where
    prettyRoot (TrieNode mval children) =
        text "\"\"" <> prettyVal mval
        $+$
        prettyChildren children

    prettyNonRoot (c, TrieNode mval children) =
        char c <> prettyVal mval
        $+$
        prettyChildren children

    prettyVal Nothing = Doc.empty
    prettyVal (Just v) = text (" : " ++ show v)

    prettyChildren = nest 2 . vcat . map prettyNonRoot . Map.toList

printTrie :: Show a => Trie a -> IO ()
printTrie = putStrLn . prettyTrie

trieIsEmpty :: Trie a -> Bool
trieIsEmpty (TrieNode Nothing children) | Map.null children = True
trieIsEmpty _ = False

albertTrie :: Trie Integer
albertTrie = TrieNode (Just 4) (Map.fromList [('a', a), ('p', p), ('t', t)])
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
