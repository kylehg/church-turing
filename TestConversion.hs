module ConversionTests where

import Church
import Turing
import Conversion
import Test.HUnit


char :: Char -> Term
char = el ['a'..'z']

string :: String -> Term
string = els ['a'..'z']


test_el :: Test
test_el = char 'a' ~?=
          (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "_" $
           var "'a'")

test_els :: Test
test_els =
  string "acd" ~?=
  (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "_" $ var "'a'" <->
   (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "_" $ var "'c'" <->
    (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "_" $ var "'d'" <->
     (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "_" $ var "_"))))

test_cons :: Test
test_cons = TestList [
  nf (cons abc <-> char 'a' <-> string "bc") ~?= string "abc"
  ]

{-
prop_conv :: TM -> String -> Bool
prop_conv m t = nf $ (execTerm m) <-> (initTerm m) tapeTerm $ listFromTape $ runTM m1 $ tapeFromList t
-}

main :: IO ()
main = do
  _ <- runTestTT $ TestList [
    test_convTape,
    test_convSym,    
    test_cons
    ]
  return ()