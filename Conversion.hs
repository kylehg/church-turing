module Conversion where

import Data.Map (Map)
import Data.List (intersperse)
import Church
import Turing
import LCs
import TMs


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

fun :: (TMState, Alphabet) -> (TMState, Move)
fun (s, w) = case (s, w) of
  (start, _) -> (
--  (nf, x') -> (nf, [R]) -- Var -> do nothing
--  (nf, x)  -> (e, [R])
--  
--  (nf,  l)  -> (lam, [R]) -- Lam: ignore var, reduce term
--  (lam, x') -> (lam, [R])
--  (lam, x)  -> (nf,  [R])
--  
--  (nf, bl) -> (wnf, [R, I '#', R]) -- App: whnf the first term
--
--  (wnf, bl) -> (wnf, [R, I '$', R]) -- Do whnf
--  (wnf, _)  -> (rwnf, [L]) -- Not an app - leave wnf
--  
--  -- Return out of a WNF call
--  (rwnf, '#') -> (nfsub, [E, I '|', R]) -- Returning to a nf call
--  (rwnf, '$') -> (wnfsub, [E, I '|', R]) -- Returning to a wnf call
--  (rwnf, _)   -> (rwnf, [L]) -- Keep returning
--  
--  -- Perform a substition as part of a wnf call
--  (wnfsub
  
--  (ret -- Return from WNF
   -- TODO: Figure out how to leave "function call"

  where
    nf = 0
    lam = 2
    e = 3
    x = 'x'
    x' = '\''
    bl = '['
    br = ']'
    l = '\\'
    h = '#'
    a = '&'
