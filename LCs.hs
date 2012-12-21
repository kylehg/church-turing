-- Some lambda calculus terms

module LCs where

import Church
import Prelude hiding (and, pred, fst, snd, succ)
--import Test.HUnit


-- | ((λx.y) (λz.(λq.z z))) ((λf.g) (λy.h))
t :: Term
t = App (App (Lam "x" (Var "y"))
             (Lam "z" (Lam "q" (App (Var "z") (Var "z")))))
        (App (Lam "f" (Var "g"))
             (Lam "y" (Var "h")))

t' = ((lam "x" $ var "y") <-> (lam "z" $ lam "q" $ (var "z") <-> (var "z")))
     <-> ((lam "f" $ var "g") <-> (lam "y" $ var "h"))

-- Church arithmatic ----------------------------------------------------------
zero, succ, mul, pred, sub :: Term

-- | zero = \f.\z.z
zero = lam "f" $ lam "z" $ var "z"

-- | succ = \n.\f.\z.f (n f z)
succ = lam "n" $ lam "f" $ lam "z" $ 
       (var "f") <-> ((var "n") <-> (var "f") <-> (var "z"))

-- | Convert an integer into a Church number
toChurch :: Int -> Term
toChurch 0 = zero
toChurch n = succ <-> toChurch (n-1)

-- | Convert a Church-encoded term to an integer.
fromChurch :: Term -> Maybe Int
fromChurch t = case (nf t) of
  Lam f (Lam z n) -> countApps n 0
  _               -> Nothing
  where countApps (App (Var _) t') c = countApps t' (c+1)
        countApps (Var _) c          = Just c
        countApps _ _                = Nothing

-- | add = \m.\n.\f.\z.m f (n f z)
add = lam "m" $ lam "n" $ lam "f" $ lam "z" $
      (var "m") <-> (var "f") <-> ((var "n") <-> (var "f") <-> (var "z"))

-- | mul = \m.\n.\f.m (n f)
mul = lam "m" $ lam "n" $ lam "f" $ (var "m") <-> ((var "n") <-> (var "f"))


-- Factorial ------------------------------------------------------------------
true, false, not, and, or, pair, fst, snd, iszero, equal, y, fact1,
 fact :: Term

-- | true = \t.\f.t
true = lam "t" $ lam "f" $ var "t"

-- | false = \t.\f.f
false = lam "t" $ lam "f" $ var "f"

-- | not = \b.b false true
not = lam "b" $ (var "b") <-> false <-> true

-- | and = \p.\q.p q false
and = lam "p" $ lam "q" $ (var "p") <-> (var "q") <-> false

-- | or = \p.\q.p true q
or = lam "p" $ lam "q" $ (var "p") <-> true <-> (var "q")


-- | pair = \a.\b.\f.f a b
pair = lam "a" $ lam "b" $ lam "f" $ (var "f") <-> (var "a") <-> (var "b")

-- | fst = \p.p true
fst = lam "p" $ (var "p") <-> true

-- | snd = \p.p false
snd = lam "p" $ (var "p") <-> false

-- | pred = \n.fst (n (\p. pair (snd p) (succ (snd p))) (pair zero zero))
pred = lam "n" $ fst <->
       ((var "n") <->
        (lam "p" $
         pair <-> (snd <-> (var "p")) <-> (succ <-> (snd <-> (var "p"))))
        <-> (pair <-> zero <-> zero))

-- | sub = \m.\n.n pred m
sub = lam "m" $ lam "n" $ (var "n") <-> pred <-> var "m"

-- | iszero = \n. n (\x.false) true
iszero = lam "n" $ (var "n") <-> (lam "x" $ false) <-> true

-- | equal = \m.\n.and (iszero (sub m n)) (iszero (sub n m))
equal = lam "m" $ lam "n" $ and <->
        (iszero <-> (sub <-> (var "m") <-> (var "n")))
        <-> (iszero <-> (sub <-> (var "n") <-> (var "m")))

-- | y = (\x.\f.f (x x f)) (\x.\f.f (x x f))
y = phi <-> phi where phi = lam "x" $ lam "f" $
                            (var "f") <-> ((var "x") <-> (var "x") <-> var "f")

-- | fact1 = \f.\n.(iszero n) one (mul n (f (pred n)))
fact1 = lam "f" $ lam "n" $ (iszero <-> (var "n")) <-> (toChurch 1) <->
        (mul <-> (var "n") <-> ((var "f") <-> (pred <-> (var "n"))))

-- | fact = y fact1
fact = y <-> fact1



-- Tests ----------------------------------------------------------------------
{-main :: IO ()
main = do
  _ <- runTestTT $ TestList [
--     "factorial" ~: 
                            ]
  return ()
-}