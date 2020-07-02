module TrieDelete where

import           TrieDef

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

trieDelete :: [Char] -> Trie a -> Trie a
trieDelete _ trie | trieIsEmpty trie = trie
trieDelete "" (TrieNode value children) = TrieNode Nothing children
trieDelete key (TrieNode value children)
    | Map.member edge children == False = trie
    | otherwise = TrieNode value newChildren
        where
            edge = head key
            trie = TrieNode value children
            child = children Map.! edge
            newTrie = trieDelete (tail key) child
            newChildren
                | trieIsEmpty newTrie = Map.delete edge children
                | otherwise = Map.adjust (\ _ -> newTrie) edge children



{-

WTP: For all [Char] aka strings: trieDelete key trie = trie - key

Base case

    WTP: trieDelete a trie where a is not in trie
    If a not in trie, then there's nothing to delete.
    So trie is returned.

-}
