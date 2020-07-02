module EdgeGeographyGame where

-- You may import useful modules here.
import           Data.Set (Set)
import qualified Data.Set as Set

{- The input is a list of adjacency lists, e.g.,
   [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
   means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.

   goodFirstVertices takes this input and computes the choices for the first
   vertex so the first player is destined to win.
-}

type Vertex = Int
type Edge = (Vertex,Vertex)
data Graph = Graph (Set Edge) deriving (Show)
data Turn = P1 | P2 deriving (Eq)
data GameState = State Turn Vertex Graph

goodFirstVertices :: [(Int, [Int])] -> [Int]
goodFirstVertices inp =
    map (\(State turn vertex graph) -> vertex) winningStates
    where
        g = Graph (Set.fromList edges)
        vertices = map fst inp
        edges = foldl (++) [] edgesList
        edgesList = map (\(x,y) -> map (\z -> (x,z)) y) inp
        startingVertex v = State P2 v g
        startingStates = map startingVertex vertices
        winningStates = filter (\gs -> fst (lastPlayerDestinedToWin gs)) startingStates


legalNextStates :: GameState -> [GameState]
legalNextStates (State turn currentVertex (Graph edges)) =
    nextStates
    where
        possibleMoves = filter (\(v,_) -> v == currentVertex) (Set.toList edges)
        nextTurn
            | turn == P1 = P2
            | otherwise  = P1
        newEdges e = Set.delete e edges
        newGraph e = Graph (newEdges e)
        nextStates = map (\(v,w) -> State nextTurn w (newGraph (v,w))) possibleMoves

lastPlayerDestinedToWin :: GameState -> (Bool, Turn)
lastPlayerDestinedToWin currentState@(State turn currentVertex (Graph edges))
    | Set.null possibleMoves = (True, lastPlayer)
    | otherwise = result
        where
            possibleMoves = Set.filter (\(v,_) -> v == currentVertex) edges
            lastPlayer
                | turn == P1 = P2
                | otherwise  = P1
            result
                | fst tempResult == True && snd tempResult == lastPlayer = tempResult
                | fst tempResult == False && snd tempResult /= lastPlayer = (True, lastPlayer)
                | otherwise = (False, lastPlayer)
            tempResult
                | (True, turn) `elem` gameResults = (True, turn)
                | otherwise = (False, turn)
            gameResults = map lastPlayerDestinedToWin nextStates
            nextStates = legalNextStates currentState
