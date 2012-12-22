module Conversion where

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Test.HUnit
import           Church
import           Turing
import           LCs
import           TMs
import Prelude hiding (id)

-- TM to LC -------------------------------------------------------------------

-- | Represent a single element in a set.
el :: Show c => [c] -> c -> Term
el cs c = iterLam cs (var $ show c)

-- | Represent a string of characters in an alphabet.
els :: Show c => [c] -> [c] -> Term
els cs s = foldr el' empty s where
  el' c t = iterLam cs (lam e $ var (show c) <-> t)
  empty = iterLam cs (lam e $ var e)
  e = "e"

-- | Iterate a set of elements as a series of lambdas ending in a term.
iterLam :: Show c => [c] -> Term -> Term
iterLam cs t = foldr (lam . show) t cs

-- | Iterate a set of applications for each element in a set, starting 
-- with a given term.
iterApp :: [c] -> Term -> (c -> Term) -> Term
iterApp cs t f = iterApp' (reverse cs) where
  iterApp' (c:cs) = (iterApp' cs) <-> (f c)
  iterApp' []     = t

-- | Like el, except applies the determining variable to a given term 
-- at the end.
appEl :: Show c => [c] -> c -> Term -> Term
appEl cs c t = iterLam cs (var (show c) <-> t)

-- | Convert a term back into a string (if it's valid).
fromTerm :: [Char] -> Term -> Maybe String
fromTerm abc = fromTerm' Map.empty abc where
  fromTerm' m (c:cs) (Lam x t)           = fromTerm' (Map.insert x c m) cs t
  fromTerm' m [] (Lam e (App (Var x) t)) = do c <- Map.lookup x m
                                              cs <- fromTerm abc t
                                              return (c:cs)
  fromTerm' m [] (Lam e (Var x))         | e == x    = Just ""
                                         | otherwise = Nothing
  fromTerm' _ _ _                        = Nothing


-- Lambdas --------------------------------------------------------------------
-- | The TM combinator. H f -> f (\z.H f z)
recurse :: Term
recurse = m <-> m where
  m = lam "x" $ lam "f" $
      var "f" <-> (lam "z" $ var "x" <-> var "x" <-> var "f" <-> var "z" )

-- | The identity lambda
id :: Term
id = lam "x" $ var "x"

-- | Cons symbol terms 
-- (cons cs) <-> (el cs a) <-> (els cs as) --> els cs (a:as)
cons :: [Char] -> Term
cons cs =
  lam "a" $ lam "as" $ iterApp cs (var "a") cons' <-> var "as" where
    cons' c = lam "as" $ iterLam cs (lam "b" $ var (show c) <-> var "as")

-- | Concatenate symbol terms
-- (cat cs) <-> (els cs a) <-> (els cs b) --> els cs (a ++ b)
cat :: [Char] -> Term
cat cs = recurse <-> cat' where
  cat' = lam "x" $ lam "a" $ lam "b" $
         iterApp cs (var "a") cat'' <-> id <-> var "b"
  cat'' c = lam "a" $ lam "b" $
            (lam "h" $ iterLam cs $ lam "g" $ var (show c) <-> var "h") <->
            (var "x" <-> var "a" <-> var "b")

tm :: TM -> Term
tm m = recurse <-> tm' where
  (qs, cs, b, q0, qe) = (states m, alpha m, blank m, start m, end m)
  tm' = lam "x" $ lam "y" $ var "y" <->
        (lam "l" $ lam "c" $ lam "r" $ lam "q" $
         iterApp qs (var "q") qTerm <-> var "l" <-> var "c" <-> var "r") 
  qTerm q = lam "l" $ lam "c" $ lam "r" $
            iterApp cs (var "c") (cTerm q) <-> var "l" <-> var "r"
  cTerm q c | q == qe = lam "l" $ lam "r" $ lam "x" $
                        var "x" <-> var "l" <-> el cs c <-> var "r" <-> el qs q
            | d == L   = lam "l" $ lam "r" $ var "x" <->
                        (iterApp cs (var "l") pc <-> p <->
                         (cons cs <-> el cs c' <-> var "r") <->
                         el qs q')
            | d == R   = lam "l" $ lam "r" $ var "x" <->
                        (iterApp cs (var "r") rc <-> r <->
                         (cons cs <-> el cs c' <-> var "l") <->
                         el qs q')
    where (q', c', d) = trans m (q, c)
  pc c = lam "l" $ lam "r" $ lam "q" $ lam "x" $
         var "x" <-> var "l" <-> el cs c <-> var "r" <-> var "q"
  p = lam "r" $ lam "q" $ lam "x" $
      var "x" <-> els cs "" <-> el cs b <-> var "r" <-> var "q"
  rc c = lam "r" $ lam "l" $ lam "q" $ lam "x" $
         var "x" <-> var "l" <-> el cs c <-> var "r" <-> var "q"
  r = lam "l" $ lam "q" $ lam "x" $
      var "x" <-> var "l" <-> el cs b <-> els cs "" <-> var "q"

  
config :: TM -> Tape -> Term
config m t = lam "x" $ var "x" <-> ls <-> c <-> rs <-> qt where
  ls = els (alpha m) (behind t)
  c = el (alpha m) (getC t)
  rs = els (alpha m) (ahead t)
  qt = el (states m) (start m)

fromConfig :: TM -> Term -> Maybe Tape
fromConfig m (Lam x (App (App (App (App (Var y) ls) c) rs) q))
  | x == y    = do l' <- fromTerm cs ls
                   r' <- fromTerm cs $ nf $ (cons cs) <-> c <-> rs
                   return $ Tape l' r'
  | otherwise = Nothing
  where cs = alpha m


testM1 :: Test
testM1 = Just (runTM m1 startTape1) ~?=
         (fromConfig m1 $ nf $ (tm m1) <-> config m1 startTape1)

main :: IO ()
main = do
  _ <- runTestTT $ TestList [
    testM1
    ]
  return ()