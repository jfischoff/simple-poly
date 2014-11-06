{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Tests.Language.SimplePoly.Unification where
import Language.SimplePoly.Unification
import Language.SimplePoly.Types
import Tests.Language.SimplePoly.Types ()
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit
import Control.Lens hiding (elements)
import Control.Lens.Plated
import Data.Data.Lens
import Test.QuickCheck
import Control.Applicative
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad
import Control.Arrow
import Data.Foldable (toList)

-- 
-- given a type 
-- another type is unified with it 
-- if the types are equal
-- or if for any type variable 
-- the type is replaced with another type

data Unifyable = Unifyable Type Type
   deriving (Show, Eq)

-- This is wrong 
-- because If I 
instance Arbitrary Unifyable where
   arbitrary = do 
      x <- arbitrary
      y <- evalStateT (go x) 
         . Subst 
         . M.fromList 
         . map (second TVar . join (,))
         . toList
         . freeTyVars 
         $ x
      return $ Unifyable x y where
         
         -- I might want to take this out to test it
         -- pass in the arbitrary
         go :: Type -> StateT Subst Gen Type 
         go = \case
                  TInt    -> return TInt
                  TVar a -> do
                     subs <- get
                     if a `S.member` boundVars subs then 
                        return $ TVar a
                     else do                     
                        newValue <- lift $ suchThat arbitrary 
                                       ( S.null 
                                       . S.intersection (boundVars subs) 
                                       . freeTyVars
                                       )
                        -- add the new generated value as a sub
                        modify ((unit a newValue) <>)
                  
                        return newValue
                  a :-> b -> do 
                     (:->) <$> go a 
                           -- apply the subs before recursing
                           <*> (go . flip sub b =<< get)


                                   
--(TVar (Ident "i") :-> TVar (Ident "i")) 
--((TInt :-> (TVar (Ident "B") :-> TInt)) :-> (TInt :-> (TVar (Ident "B") :-> TInt)))
                                   
{-
   shrink (Unifyable x y) = case x of
      TInt    -> []
      TVar n  -> map (Unifyable (TVar n)) $ shrink y
      a :-> b -> case y of
         c :-> d -> [Unifyable a c, Unifyable b d] ++ do 
            Unifyable g h <- shrink $ Unifyable a c
            Unifyable l m <- shrink $ Unifyable b d
            return $ Unifyable (g :-> l) (h :-> m) 
         _ -> error "impossible Unifyable value"
-}
         
case_base_case_unifier_definition = 
   mgu (TVar $ Ident "a") (TVar $ Ident "b") @?= 
      Right (Subst $ M.fromList [(Ident "a", TVar $ Ident "b")])
      

--case_simple_unification_0 = 
   

-- s is a unifier if forall x and y -> apply s x == apply s y
prop_unifier_definition :: Unifyable -> Bool
prop_unifier_definition (Unifyable x y) =  
   case mgu x y of
      Left _  -> False
      Right s -> sub s x == sub s y
      
-- The most general unifier
-- forall mgu and any other unifier
-- I don't see how to easily test this

tests :: TestTree
tests = $testGroupGenerator