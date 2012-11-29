-- Code for representing lambda calculus terms in Haskell
-- Author: Kyle Hardgrave (kyleh@seas)

module Church where



type Name = String

data Term = Var Name
          | Lam Name Term
          | App Term Term
          deriving Eq

instance Show Term where
  show (Var n)     = n
  show (Lam n t)   = "(λ" ++ n ++ "." ++ (show t) ++ ")"
  show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"


-- | ((λx.y) (λz.(λq.z z))) ((λf.g) (λy.h))
t :: Term
t = App (App (Lam "x" (Var "y"))
             (Lam "z" (Lam "q" (App (Var "z") (Var "z")))))
        (App (Lam "f" (Var "g"))
             (Lam "y" (Var "h")))

