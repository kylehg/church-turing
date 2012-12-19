module Conversion where

import qualified Data.Set as Set
import           Test.HUnit
import           Church
import           Turing
import           LCs
import           TMs


-- | Given a LC term and a term to apply it to, convert them into a TM
-- and tape to run it on, respectively.
lcToTM :: Term -> Tape
lcToTM = tapeFromList . lcToTM' where
  lcToTM' (Var n)     = varToTape n
  lcToTM' (Lam n t)   = "\\" ++ (varToTape n) ++ lcToTM' t
  lcToTM' (App t1 t2) = "[" ++ (lcToTM' t1) ++ "][" ++ (lcToTM' t2) ++ "]"


-- | Covert a variable to a string printable on a TM tape.
varToTape :: Name -> String
varToTape n = varMap' n allNames where
  varMap' n (n0:ns) | n == n0   = "x"
                    | otherwise = '\'' : (varMap' n ns)


-- TM to LC -------------------------------------------------------------------

-- | Given an alphabet, convert a tape of symbols to an LC term.
convTape :: Set.Set Char -> String -> Term
convTape cs s = foldr convChar b s where
  convChar c t = foldr lam (var [c] <-> t) $ Set.elems cs
  -- TODO: In the base case, we apply the ID function. Is that right?
  b = lam "" $ var ""


main :: IO ()
main = do
  _ <- runTestTT $ TestList [
    --test_symToLC
    ]
  return ()