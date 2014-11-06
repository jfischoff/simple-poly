{-# LANGUAGE TemplateHaskell #-}
module Tests.Language.SimplePoly where
import Tests.Language.SimplePoly.Types as Types
import Tests.Language.SimplePoly.Infer as Infer
import Tests.Language.SimplePoly.Unification as Unification 
import Test.Tasty
import Test.Tasty.TH
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "SimplePoly" 
        [ Types.tests
        , Infer.tests
        , Unification.tests
        ]