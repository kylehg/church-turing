-- Code for representing and interpreting lambda calculus terms in Haskell.
-- Author: Kyle Hardgrave (kyleh@seas)

module Church where

import           Control.Applicative
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.QuickCheck


type Name = String
data Term = Var Name
          | Lam Name Term
          | App Term Term

instance Show Term where
  show (Var n)     = n
  show (Lam n t)   = "(λ" ++ n ++ "." ++ (show t) ++ ")"
  show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"

instance Eq Term where
  -- I've defined term equality as alpha-equality.
  (==) = aEq Map.empty where
    aEq :: Map.Map Name Name -> Term -> Term -> Bool
    aEq m (App t1 t1') (App t2 t2') = (aEq m t1 t2) && (aEq m t1' t2')
    aEq m (Lam n1 t1)  (Lam n2 t2)  = aEq (Map.insert n1 n2 m) t1 t2
    aEq m (Var n1)     (Var n2)     = maybe False (== n2) (Map.lookup n1 m)
    aEq _ _ _                       = False

instance Arbitrary Term where
  -- Borrowed from Brent Yorgey's LC interpreter. See /reading.
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


-- Basic functions for reducing LC terms. Mostly taken or modified
-- from Lennart Augustsson (see /reading).

-- | Get all free variable names from a term.
freeVars :: Term -> Set.Set Name
freeVars (Var n)     = Set.singleton n
freeVars (Lam n t)   = Set.delete n $ freeVars t
freeVars (App t1 t2) = (freeVars t1) `Set.union` (freeVars t2)

-- | Get all the variables names in a term.
allVars :: Term -> Set.Set Name
allVars (Var v)     = Set.singleton v
allVars (Lam _ t)   = allVars t
allVars (App t1 t2) = allVars t1 `Set.union` allVars t2

-- | Reduce a term to normal form (beta-reduction).
nf :: Term -> Term
nf t@(Var _)   = t
nf (Lam n t)   = Lam n $ nf t
nf (App t1 t2) = case whnf t1 of
  Lam n t1' -> nf $ subst n t2 t1'
  t1'       -> App (nf t1') (nf t2)

-- | Compute weak-head normal form - similar to beta-reduction, but
-- only reduces terms of the form, (\x.t1) t2
whnf :: Term -> Term
whnf t@(Var _)   = t
whnf t@(Lam _ _) = t
whnf (App t1 t2) = case whnf t1 of
  Lam n t1' -> whnf $ subst n t2 t1'
  t1'       -> App t1' t2
  
-- | Substitute a term `t'` for a variable named `x` in `t0`.
subst :: Name -> Term -> Term -> Term
subst x t' t0 = sub t0 where
  sub t@(Var n)    | n == x    = t'
                   | otherwise = t
  sub t@(Lam n t1) | n == x            = t
                   | n `Set.member` fs = Lam n' $ sub t''
                   | otherwise         = Lam n (sub t1) where
                     n' = newVar vs
                     t'' = subst n (Var n') t'
  sub (App t1 t2) = App (sub t1) (sub t2)
  fs = freeVars t'
  vs = fs `Set.union` allVars t0

-- | Return a variable name not in a set of existing variable names.
newVar :: Set.Set Name -> Name
newVar vs = case (find (`Set.member` vs) allNames) of
  Just var -> var
  Nothing  -> error "Impossible: infinite possible var names."

-- | All possible variable names: [a...z, a'...z', a''...]
allNames :: [Name]
allNames = [x:y | y <- [replicate n '\'' | n <- [0..]]
                , x <- ['a'..'z']]
