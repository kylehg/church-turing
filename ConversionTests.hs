module ConversionTests where

import Church
import Turing
import Conversion
import Test.HUnit


test_convTape :: Test
test_convTape =
  convTape "abcd" "acd" ~?=
  (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $ (var "'a'") <->
   (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $ (var "'c'") <->
    (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $ (var "'d'") <->
     (lam "'a'" $ lam "'b'" $ lam "'c'" $ lam "'d'" $ lam "__" $ (var "__")))))



{-
prop_conv :: TM -> String -> Bool
prop_conv m t = nf $ (execTerm m) <-> (initTerm m) tapeTerm $ listFromTape $ runTM m1 $ tapeFromList t
-}

main :: IO ()
main = do
  _ <- runTestTT $ TestList [
    test_convTape
    ]
  return ()