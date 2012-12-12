module TMs where

import qualified Data.Set as Set
import Data.List
import Test.HUnit
import Turing


main :: IO ()
main = do
  _ <- runTestTT $ TestList [test1]
  return ()


-- A turing machine to decide the language of all strings of 0s of
-- length of a power of 2.
trans1 :: (TMState, Alphabet) -> (TMState, Move)
trans1 (s, w) = case (s, w) of
  (1, '_') -> (e, [R])
  (1, 'x') -> (e, [R])
  (1, '0') -> (2, [P '_', R])

  (2, 'x') -> (2, [R])
  (2, '_') -> (e, [R])
  (2, '0') -> (3, [P 'x', R])

  (3, 'x') -> (3, [R])
  (3, '0') -> (4, [R])
  (3, '_') -> (5, [L])

  (4, 'x') -> (4, [R])
  (4, '0') -> (3, [P 'x', R])
  (4, '_') -> (e, [R])

  (5, '0') -> (5, [L])
  (5, 'x') -> (5, [L])
  (5, '_') -> (2, [R])

  (_, w)   -> (e, [R])
  where e = 6

m1 :: TM
m1 = TM {
  trans  = trans1,
  start  = 1,
  end    = 6,
  states = Set.fromList [1..6]
  }

test1 :: Test
test1 = (listFromTape $ runTM m1 $ tapeFromList "0000") ~?= "_xxx_"


-- A turing machine to compute 001011011101111...
-- TODO: Nonterminating TMs
--trans2 :: (TMState, Alphabet) -> (TMState, Move)
--trans2 (s, w) = case (s, w) of
--  (b, _)   -> (o, [P 'e', R, P 'e', R, P '0', R, R, P '0', L, L])
--
--  (o, '1') -> (o, [R, P 'x', L, L, L])
--  (o, '0') -> (q, [P '0'])
--
--  (q, '_') -> (p, [P '1', L])
--  (q, _)   -> (q, [R, R])
--
--  (p, 'x') -> (q, [P '_', R])
--  (p, 'e') -> (f, [R])
--  (p, '_') -> (p, [L, L])
--
--  (f, '_') -> (o, [P '0', L, L])
--  (f, _)   -> (f, [R, R])
--  where b = 0
--        o = 1
--        q = 2
--        p = 3
--        f = 4
--
--m2 :: TM
--m2 = TM {
--  trans  = trans2,
--  start  = 0,
--  end    = Nothing,
--  states = Set.fromList [0..4]
--  }
--
--test2 :: Test
--test2 = (take 20 $ listFromTape $ runTM m2 $ tapeFromList "ee0")
--        ~?= "ee" ++ intersperse '_' "001011011101111011111"