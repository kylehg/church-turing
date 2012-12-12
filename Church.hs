-- Code for representing lambda calculus terms in Haskell
-- Author: Kyle Hardgrave (kyleh@seas)

module Church where

import Control.Applicative
import qualified Data.Map as Map
import Data.List (find)
import Data.Set (Set, delete, member, singleton, union)
import Test.QuickCheck


type Name = String
data Term = Var Name
          | Lam Name Term
          | App Term Term

instance Show Term where
  show (Var n)     = n
  show (Lam n t)   = "(λ" ++ n ++ "." ++ (show t) ++ ")"
  show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"

instance Eq Term where
  (==) = alphaEq Map.empty where
    aEq :: Map.Map Name Name -> Term -> Term -> Bool
    aEq m (App t1 t1') (App t2 t2') = (aEq m t1 t2) && (aEq m t1' t2')
    aEq m (Lam n1 t1)  (Lam n2 t2)  = aEq (Map.insert n1 n2 m) t1 t2
    aEq m (Var n1)     (Var n2)     = maybe False (== n2) (Map.lookup n1 m)
    aEq _ _ _                       = False

instance Arbitrary Term where
  arbitrary = sized term where
    v = (:[]) <$> elements ['p'..'z']
    term 0 = var <$> v
    term n = oneof [lam <$> v <*> term (n-1),
                    (<->) <$> term (n `div` 2) <*> term (n `div` 2)]

-- Some helpers
lam :: String -> Term -> Term
lam = Lam

(<->) :: Term -> Term -> Term
t1 <-> t2 = App t1 t2

var :: String -> Term
var = Var


-- Basic functions for reducing LC terms. Mostly taken from Lennart
-- Augustsson (see reading).

-- | Get free variables from a term.
freeVars :: Term -> Set Name
freeVars (Var n)     = singleton n
freeVars (Lam n t)   = delete n $ freeVars t
freeVars (App t1 t2) = (freeVars t1) `union` (freeVars t2)

-- | Get all the variables in a term.
allVars :: Term -> Set Name
allVars (Var v)   = singleton v
allVars (Lam _ e) = allVars e
allVars (App f a) = allVars f `union` allVars a

-- | Reduce a term to normal form (beta-reduction).
nf :: Term -> Term
nf e@(Var _) = e
nf (Lam x e) = Lam x (nf e)
nf (App f a) = case whnf f of
  Lam x b -> nf (subst x a b)
  f'      -> App (nf f') (nf a)

-- | Compute weak-head normal form
whnf :: Term -> Term
whnf e@(Var _)   = e
whnf e@(Lam _ _) = e
whnf (App f a)   = case whnf f of
  Lam x b -> whnf (subst x a b)
  f' -> App f' a
  
-- | Substitute a term `s` for a variable named `x` in `b`.
subst :: Name -> Term -> Term -> Term
subst x new = sub where
  sub t@(Var n)    | n == x    = new
                   | otherwise = t
  sub t@(Lam n t') | n == x       = t
                   | member n fvs = Lam v' (sub e'')
                   | otherwise    = Lam n (sub e') where
                     v' = newVar vs
                     e'' = subst n (Var v') e'
  sub (App f a)    = App (sub f) (sub a)
  fvs = freeVars new
  vs = fvs `union` allVars b

-- | Return a variable name not in a set of existing variable names.
newVar :: Set Name -> Name
newVar vs = case (find (\v -> member v vs) allNames) of
  Just var -> var
  Nothing  -> error "Impossible: infinite possible var names."

-- | All possible variable names
allNames :: [Name]
allNames = [x:y | y <- [replicate n '\'' | n <- [0..]], x <- ['a'..'z']]
