module TMs where

import qualified Data.Set as Set
import Test.HUnit
import Turing


main :: IO ()
main = do
  _ <- runTestTT $ TestList [test1, test2]
  return ()


-- A turing machine to decide the language of all strings of 0s of 
-- length of a power of 2.
trans1 :: (TMState, Alphabet) -> (TMState, Alphabet, Dir)
trans1 (s, w) = case (s, w) of
  (1, '_') -> (e, 'f', R)
  (1, 'x') -> (e, 'f', R)
  (1, '0') -> (2, '_', R)
  
  (2, 'x') -> (2, 'x', R)
  (2, '_') -> (e, 's', R)
  (2, '0') -> (3, 'x', R)
  
  (3, 'x') -> (3, 'x', R)
  (3, '0') -> (4, '0', R)
  (3, '_') -> (5, '_', L)
  
  (4, 'x') -> (4, 'x', R)
  (4, '0') -> (3, 'x', R)
  (4, '_') -> (e, 'f', R)
  
  (5, '0') -> (5, '0', L) 
  (5, 'x') -> (5, 'x', L)
  (5, '_') -> (2, '_', R)
  
  (_, w)   -> (e, w, R)
  where e = 6

m1 :: TM
m1 = TM {
  trans  = trans1,
  start  = 1,
  end    = 6,
  states = [1..6],
  alpha  = "_0xsf",
  blank  = '_'
  }


startTape1 :: Tape
startTape1 = tapeFromList "0000"

startTape2 :: Tape
startTape2 = tapeFromList "000"

result1 :: String
result1 = listFromTape $ runTM m1 startTape1

result2 :: String
result2 = listFromTape $ runTM m1 startTape2

test1 :: Test
test1 = "_xxxs" ~?= result1

test2 :: Test
test2 = "_x0f" ~?= result2
