{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Tests.Language.SimplePoly.Types where
import Language.SimplePoly.Types 
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit
import Control.Lens hiding (elements, Context)
import Control.Lens.Plated
import Data.Data.Lens
import Test.QuickCheck
import Control.Applicative
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error
import Control.Arrow
import Data.List

tests :: TestTree
tests = $testGroupGenerator

instance Arbitrary Ident where
   arbitrary = Ident <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'B'])

instance Arbitrary Type where
   arbitrary = sized go where
      go size = oneof $
         [ return TInt
         , TVar <$> arbitrary
         ] ++ if size > 0 then
                  [(:->) <$> go (size `div` 2) <*> go (size `div` 2)]
              else
                  []
   shrink = \case 
      x :-> y -> [x, y]
      _ -> []

instance Arbitrary TypedExpr where
   arbitrary = sized $ \s -> evalStateT arbitraryExpr (s, mempty)

type Context        = StateT (Int, [(Sym, Type)]) Gen
type PartialContext = ErrorT String (StateT (Int, [(Sym, Type)]) Gen)

retryOnError :: PartialContext a -> Context a
retryOnError action = either (const (retryOnError action)) return
                    =<< runErrorT action

arbitraryExpr :: Context TypedExpr
arbitraryExpr = retryOnError (arbitraryAEExprWith =<< lift (lift arbitrary))

arbitraryAEExprWith :: Type -> PartialContext TypedExpr
arbitraryAEExprWith e = do
   size <- gets fst
   let maxOption = if size > 0 then 1 else 0
   opt <- lift $ lift $ choose (0 :: Int, maxOption)
   case e of 
      TInt   -> case opt of
                  0 -> lift arbitraryAELit  
                  1 -> arbitraryAEApWith TInt
      -- Type vars can only be introduced through lambda
      -- abstraction
      -- so look for a Var with the type TVar x
      TVar x -> case opt of
                  0  -> do
                    mresult <- gets (find (maybe False (==x) . preview (_2 . _TVar)) . snd)
                    case mresult of
                       Nothing -> throwError ""
                       Just m -> return $ AEVar (TVar x) . fst $ m
                  1 -> arbitraryAEApWith $ TVar x
      -- Another possibility is that it is a AEAp
      x :-> y -> arbitraryAEAbsWith x y

arbitraryAELit :: Context TypedExpr
arbitraryAELit = AELit TInt <$> lift arbitrary

arbitraryAEVar :: Context TypedExpr
arbitraryAEVar = fmap (uncurry (flip AEVar)) . lift . elements =<< gets snd

arbitraryAEAbs :: Context TypedExpr
arbitraryAEAbs = retryOnError $ do 
   x <- lift $ lift arbitrary
   y <- lift $ lift arbitrary 
   arbitraryAEAbsWith x y

arbitraryAEAbsWith :: Type -> Type -> PartialContext TypedExpr
arbitraryAEAbsWith symType bodyType = do
   modify (first (\x -> x - 1))
   tys <- gets (map fst . snd)
   sym <- lift . lift $ suchThat arbitrary (not . flip elem tys)
   modify (second ((sym, symType):))
   body <- arbitraryAEExprWith bodyType
   return $ AEAbs (symType :-> bodyType) sym body

arbitraryAEAp :: Context TypedExpr
arbitraryAEAp = do
   modify $ first (`div` 2)
   retryOnError (arbitraryAEApWith =<< lift (lift arbitrary)) 

-- Make a arbitrary Ap expression
-- The result type is passed in but the inner type is unknown

arbitraryAEApWith :: Type -> PartialContext TypedExpr
arbitraryAEApWith typ = do
   inputType <- lift $ lift arbitrary
   f         <- arbitraryAEAbsWith  inputType typ
   result    <- arbitraryAEExprWith inputType
   return $ AEAp typ f result

instance Arbitrary Subst where
   arbitrary = Subst . M.fromList <$> arbitrary

case_freeVars_base_case_TInt = mempty @?= freeTyVars TInt
case_freeVars_base_case_TVar = 
   S.singleton (Ident "a") @?= freeTyVars (TVar $ Ident "a")

prop_freeVars_inductive :: Type -> Type -> Bool
prop_freeVars_inductive x y = 
   freeTyVars x <> freeTyVars y == freeTyVars (x :-> y)

prop_sub_base_case_TInt :: Subst -> Bool
prop_sub_base_case_TInt s = TInt == sub s TInt

case_sub_base_case_TVar_existing_sub = 
   TVar (Ident "b") @?= 
      sub (Subst $ M.fromList [(Ident "a", TVar (Ident "b"))]) (TVar $ Ident "a")

case_sub_base_case_TVar_no_sub = 
   TVar (Ident "a") @?= 
      sub (Subst $ M.fromList [(Ident "c", TVar (Ident "b"))]) (TVar $ Ident "a")

prop_sub_inductive :: Type -> Type -> Subst -> Bool
prop_sub_inductive x y s = 
   sub s x :-> sub s y == sub s (x :-> y)

-- I need to test the substitute 
-- I don't completely understand yet what the <> of Subst is supposed to do
-- I will need to 

-- I should test the monoid laws 

prop_monoid_right_absorbtion :: Subst -> Bool
prop_monoid_right_absorbtion x = x == x <> mempty

prop_monoid_left_absorbtion :: Subst -> Bool
prop_monoid_left_absorbtion x = x <> mempty == x

prop_monoid_associativity :: Subst -> Subst -> Subst -> Bool
prop_monoid_associativity x y z = (x <> y) <> z == x <> (y <> z)

prop_sub_append_equals_sub_in_sequence :: Subst -> Subst -> Type -> Bool
prop_sub_append_equals_sub_in_sequence x y t = 
   sub (x <> y) t == (sub x . sub y) t







