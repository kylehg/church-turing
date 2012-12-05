-- Some lambda calculus terms

-- | ((λx.y) (λz.(λq.z z))) ((λf.g) (λy.h))
t :: Term
t = App (App (Lam "x" (Var "y"))
             (Lam "z" (Lam "q" (App (Var "z") (Var "z")))))
        (App (Lam "f" (Var "g"))
             (Lam "y" (Var "h")))

