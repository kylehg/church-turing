module Conversion where

import Data.Map (Map)
import Church
import Turing


-- | Given a LC term and a term to apply it to, convert them into a TM
-- and tape to run it on, respectively.
lcToTM :: Term -> Tape
lcToTM = tapeFromList . lcToTM' where
  lcToTM' (Var n)     = varToTape n
  lcToTM' (Lam n t)   = "\\" ++ (varToTape n) ++ "[" ++ lcToTM' t ++ "]"
  lcToTM' (App t1 t2) = "[" ++ (lcToTM' t1) ++ "][" ++ (lcToTM' t2) ++ "]"


-- | Covert a variable to a string printable on a TM tape.
varToTape :: Name -> String
varToTape n = varMap' n allNames where
  varMap' n (n0:ns) | n == n0   = "x"
                    | otherwise = '\'' : (varMap' n ns)


fun :: (TMState, Alphabet) -> (TMState, Alphabet, Dir)
fun (s, w) = case (s, w) of
  -- Var -> do nothing
  (nf, '\'') -> (nf, '\'', R)
  (nf, 'x') -> (e, 'x', R)
  
  -- Lam: reduce term
  (nf, '\\') -> (readlam, '\\', R)
  (readlam, '\'') -> (readlam, '\'', R)
  (readlam, 'x') -> (nf, 'x', R)
  
  -- App:
  where
    nf = 0
    readlam = 2
    e = 3
  