module Conversion where

import qualified Data.Set as Set
import           Test.HUnit
import           Church
import           Turing
import           LCs
import           TMs


-- TM to LC -------------------------------------------------------------------

-- | Given an alphabet, convert a tape of symbols to an LC term.
-- Here, we represent the tape as
convTape :: Set.Set Char -> String -> Term
convTape cs = foldr convChar (lam "" $ var "") where
  convChar c t = foldr lam (var [c] <-> t) $ Set.elems cs
  -- TODO: In the base case, we apply the ID function. Is that right?


main :: IO ()
main = do
  _ <- runTestTT $ TestList [
    --test_symToLC
    ]
  return ()