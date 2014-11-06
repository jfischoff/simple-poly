module Main where
import Test.Tasty
import Test.Tasty.TH
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Tests.Language.SimplePoly

main = defaultMain tests 