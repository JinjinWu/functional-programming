{- Compile with

     ghc -O2 testEdgeGeographyGame.hs

   so it is not slow.
-}
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import EdgeGeographyGame (goodFirstVertices)

sortedGFV :: [(Int, [Int])] -> [Int]
sortedGFV = sort . goodFirstVertices

tests =
    [ sortedGFV [(0,[1,2]), (1, [3,4]), (2,[0]), (3,[4]), (4,[])] ~?= [0,2,4]
    , sortedGFV [(0,[2,5]),(1,[0,3,4]),(2,[0,1,4,5]),
                 (3,[1,4,5]),(4,[0,1,2,5]),(5,[1,2,4])] ~?= [2,4]
    , sortedGFV long25 ~?= [0,2,4,6,7,16,18,20,23,24]
    , sortedGFV long30 ~?= [12,15,17,20,21,22,25,26,27]
    -- more tests during marking
    ]

-- My solution takes 0.22s.
long25 = [(0,[10,11,12]),(1,[3,8,20]),(2,[9,15]),(3,[2,5,10,11]),(4,[5,11,21]),(5,[6,18,20]),(6,[3,12,14,18,19]),(7,[19]),(8,[2,13,22]),(9,[1,13,23]),(10,[5,12,16]),(11,[12,18]),(12,[7]),(13,[4,15,19,23,24]),(14,[5,6,7,15,20,23]),(15,[20,23]),(16,[9]),(17,[12,18]),(18,[6,19]),(19,[6,11,12,24]),(20,[]),(21,[1,10,14,16]),(22,[5,8,11,16]),(23,[14]),(24,[1,11])]

-- My solution takes 0.64s.
long30 = [(0,[10,12,13]),(1,[3,17]),(2,[3,6,8,22]),(3,[5,15,24]),(4,[17,27]),(5,[12,20,26]),(6,[27]),(7,[1,15,21]),(8,[1,25,28]),(9,[2,15,28]),(10,[0,2,17,19,25]),(11,[15,18,19,26]),(12,[]),(13,[6,12,17,24,26]),(14,[11,21,22,23]),(15,[5,16,18,29]),(16,[2,21]),(17,[18]),(18,[2,12,22]),(19,[21,22,24]),(20,[2,16,18,22,29]),(21,[10]),(22,[0,4,5,14,20]),(23,[2,5,10,12,14]),(24,[14,17]),(25,[5,11,23]),(26,[5,14,19]),(27,[13,18,28]),(28,[12,24,29]),(29,[2,5,9,22])]

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
