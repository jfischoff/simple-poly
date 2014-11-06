{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Language.SimplePoly.Infer where
import Language.SimplePoly.Infer
import Language.SimplePoly.Types
import Tests.Language.SimplePoly.Types hiding (tests) 
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit
import Control.Lens
import Data.Monoid
import Control.Monad
import Control.Arrow

-- generate a type
-- generate 

-- 

case_lit_base   = Right TInt @?= infer (Lit 1)

case_bound_base = Right TInt @?= runEnvWith (subs <>~ unit "a" TInt $ mempty) 
                                            (infer' (Var "a"))

case_unbound_lookup_base = 
   Left (LookupFailed "b") @?= runEnvWith (subs <>~ unit "a" TInt $ mempty) 
                                           (infer' (Var "b"))
                      
-- I need 
-- I can't remember but I think the issue was that I didn't have a good way to 
-- do alpha equivalentence                                          
prop_ap_infer_inductive :: TypedExpr -> Bool 
prop_ap_infer_inductive x = 
   uncurry (==) $ (join . curry) (infer . eraseAnnotation *** Right . view annotation) x

tests :: TestTree
tests = $testGroupGenerator