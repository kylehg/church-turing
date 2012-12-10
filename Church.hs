-- Code for representing lambda calculus terms in Haskell
-- Author: Kyle Hardgrave (kyleh@seas)

module Church where

import Control.Applicative
import Data.Set (Set, delete, member, singleton, union)
import Data.List (find)
import Test.QuickCheck


type Name = String
data Term = Var Name
          | Lam Name Term
          | App Term Term

instance Show Term where
  show (Var n)     = n
  show (Lam n t)   = "(λ" ++ n ++ "." ++ (show t) ++ ")"
  show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"

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


-- Basic functions
-- | Rename bound variables as necessary (resulting in an α-equivalent
-- term) so that no free variable has the same name as any bound variable.
-- rename :: Name -> Term -> Term
-- rename n t@(Var v) | n == v    = Var ('`':v)
--                    | otherwise = t
-- rename n (Lam v t) | n == v    = error "This shouldn't happen?"
--                    | otherwise = undefined

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

-- | Substitute one variable name for another in a term.
-- subst :: Name -> Name -> Term -> Term
-- subst n n' v@(Var n0)   | n0 == n   = Var n'
--                         | otherwise = v
-- subst n n' l@(Lam n0 t) | n0 == n   = Lam n' $ subst n n' l
--                         | otherwise = l
-- subst n n' (App t1 t2)  = App (subst n n' t1) (subst n n' t2)

-- | Apply recursive β-reduction to a LC term.
--reduce :: Term -> Term
--reduce v@(Var n)          = v
--reduce (Lam n t)          = Lam n $ reduce t
--reduce (App t1 t2) = case t1 of
--  App t1' t2' -> reduce $ App (reduce t1') (reduce t2')
--  Lam n t     -> substitute n (reduce t) (reduce t2)
--  v           -> v


nf :: Term -> Term
nf e@(Var _) = e
nf (Lam x e) = Lam x (nf e)
nf (App f a) = case whnf f of
  Lam x b -> nf (subst x a b)
  f'      -> App (nf f') (nf a)

whnf :: Term -> Term
whnf e@(Var _)   = e
whnf e@(Lam _ _) = e
whnf (App f a)   = case whnf f of
  Lam x b -> whnf (subst x a b)
  f' -> App f' a
  
-- | Substitute a term `s` for a variable named `n` in `t`.
subst :: Name -> Term -> Term -> Term
subst x s b = sub b where
  sub e@(Var v)    | v == x    = s
                   | otherwise = e
  sub e@(Lam v e') | v == x       = e
                   | member v fvs = Lam v' (sub e'')
                   | otherwise    = Lam v (sub e') where
                     v' = newVar vs
                     e'' = subst v (Var v') e'
  sub (App f a)    = App (sub f) (sub a)
  fvs = freeVars s
  vs = fvs `union` allVars b

(|->) :: Name -> Term -> Term -> Term
(|->) = subst

-- | Return a variable name not in a set of existing variable names.
newVar :: Set Name -> Name
newVar vs = case (find (\v -> member v vs) allNames) of
  Just var -> var
  Nothing  -> error "Impossible: infinite possible var names."
  where 
    allNames = [x:y | y <- [replicate n '\'' | n <- [0..]], x <- ['a'..'z']]