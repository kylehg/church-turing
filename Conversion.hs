module Conversion where

import qualified Data.Set as Set
import           Test.HUnit
import           Church
import           Turing
import           LCs
import           TMs


-- TM to LC -------------------------------------------------------------------

-- | Given an alphabet and a lambda term, encode ther term in the body of a
-- lambda for that alphabet.
convSymWith :: Show a => [a] -> Term -> Term
convSymWith cs t = foldr (lam . show) (lam "__" $ t) $ cs

convSymApp cs c t = convSymWith cs $ var (show c) <-> t

-- | Given an alphabet, convert a tape of symbols to an LC term.
convTape :: String -> String -> Term
convTape cs = foldr (\c t -> convSymApp cs c t) (convSymWith cs $ var "__")

convTMConfig :: String -> [Int] -> (Tape, Int) -> Term
convTMConfig cs qs (t, q) =
  lam "x" $ var "x" <-> (convTape cs $ behind t) <-> 
  (convSym cs $ getC t) <-> (convTape cs $ ahead t) <-> (convSym qs q)

convSym :: Show a => [a] -> a -> Term
convSym cs c = convSymWith cs (var $ show c)
  

-- | The Y-combinator. Y f -> f (Y f)
y :: Term
y = f <-> f where
  f = lam "x" $ lam "f" $ var "f" <-> (var "x" <-> var "x" <-> var "f")

-- | The TM combinator. H f -> f (\z.H f z)
h :: Term
h = m <-> m where
  m = lam "x" $ lam "f" $
      var "f" <-> (lam "z" $ var "x" <-> var "x" <-> var "f" <-> var "z" )

-- | Concatenate symbol terms
cons :: String -> Term
cons cs = lam "c" $ lam "cs" $ var "c" <-> foldr cons' (var "cs") cs where
  cons' c t = (lam "cs" $ convSymApp cs c $ var "cs") <-> t
{-
initTerm :: TM -> Term
initTerm m = h <-> lam "x" $ lam "u" $ var "u" <-> as where
  as = foldr a b (alpha m)
  b = convTMConfig (alpha m) (states m) (Tape "" "_", start m)
  a c t = lam "u" $ (var "x" <-> var "u") <->
          (lam "u" $ lam "a" $ lam "v" $ lam "q" $ 
           (lam "w" (lam "x" $ 
                     var "x" <-> var "u" <-> c' <-> var "w" <-> var "q"))
           <-> ((ac $ alpha m) <-> var "a" <-> var "v")) <-> t where
            c' = convSym (alpha m) $ var [c]
-}

--runTerm :: TM -> Term



