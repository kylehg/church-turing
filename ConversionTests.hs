module ConversionTests where

import Church
import Turing
import Conversion
import Test.HUnit

abc = "abcd"

test_convSym :: Test
test_convSym = convSym abc 'a' ~?=
               (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $
                (var "'a'"))

test_convTape :: Test
test_convTape =
  convTape abc "acd" ~?=
  (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $ (var "'a'") <->
   (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $ (var "'c'") <->
    (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $
     (var "'d'") <->
     (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $
      (var "__")))))

test_cons :: Test
test_cons = TestList [
  ((cons abc <-> (convSym abc 'a') <-> (convTape abc "bc"))) ~?=
  ((convTape abc "abc"))
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