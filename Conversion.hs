module Conversion where

import qualified Data.Set as Set
import qualified Data.Map as Map
--import           Test.HUnit
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

iterApp :: [c] -> Term -> (c -> Term) -> Term
iterApp cs t f = iterApp' (reverse cs) where
  iterApp' (c:cs) = (iterApp' cs) <-> (f c)
  iterApp' []     = t

appEl :: Show c => [c] -> c -> Term -> Term
appEl cs c t = iterLam cs (var (show c) <-> t)

-- |
--iterApp :: Show c => [Term] -> Term -> Term
--iterApp ts t = foldl (<->) t ts where
  
-- | Cons symbol terms 
-- (cons cs) <-> (el cs a) <-> (els cs as) --> els cs (a:as)
cons :: [Char] -> Term
cons cs =
  lam "a" $ lam "as" $ iterApp cs (var "a") cons' <-> var "as" where
    cons' c = lam "as" $ iterLam cs (lam "b" $ var (show c) <-> var "as")

fromTerm :: [Char] -> Term -> Maybe String
fromTerm abc = fromTerm' Map.empty abc where
  fromTerm' m (c:cs) (Lam x t)           = fromTerm' (Map.insert x c m) cs t
  fromTerm' m [] (Lam e (App (Var x) t)) = do c <- Map.lookup x m
                                              cs <- fromTerm abc t
                                              return (c:cs)
  fromTerm' m [] (Lam e (Var x))         | e == x    = Just ""
                                         | otherwise = Nothing
  fromTerm' _ _ _                        = Nothing

abc = "abcd"
a = el abc 'b'
as = els abc "acd"
s1 = nf $ cons abc <-> a <-> as
s2 = els abc "bacd"
s3 = nf $ cat abc <-> as <-> as
s4 = els abc "acdacd"


id :: Term
id = lam "x" $ var "x"

-- | Concatenate symbol terms
-- (cat cs) <-> (els cs a) <-> (els cs b) --> els cs (a ++ b)
cat :: [Char] -> Term
cat cs = recurse <-> cat' where
  cat' = lam "x" $ lam "a" $ lam "b" $
         iterApp cs (var "a") cat'' <-> id <-> var "b"
  cat'' c = lam "a" $ lam "b" $
            (lam "h" $ iterLam cs $ lam "g" $ var (show c) <-> var "h") <->
            (var "x" <-> var "a" <-> var "b")

config :: TM -> Tape -> TMState -> Term
config m t q = lam "x" $ var "x" <-> ls <-> c <-> rs <-> qt where
  ls = els (alpha m) (behind t)
  c = el (alpha m) (getC t)
  rs = els (alpha m) (ahead t)
  qt = el (states m) q
  

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
recurse :: Term
recurse = m <-> m where
  m = lam "x" $ lam "f" $
      var "f" <-> (lam "z" $ var "x" <-> var "x" <-> var "f" <-> var "z" )

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



