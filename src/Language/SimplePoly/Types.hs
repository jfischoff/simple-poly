{-
    The Polymorphic Lambda Calculus has two sorts, Type and Expr.

-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE StandaloneDeriving     #-}
module Language.SimplePoly.Types where
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Control.Lens
import Control.Lens.TH
import Data.Set.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Language.LBNF
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar
import Control.Applicative
import Debug.Trace
import Control.Monad.Error
import Data.Foldable hiding (elem)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Data
--import Prelude hiding (elem)
import Data.String

traceMsg :: Show a => String -> a -> a 
traceMsg msg x = trace (msg ++ show x) x

-- TODO
-- Make declarations 
-- and functions
-- where clauses
-- data types
bnfc [lbnf|
   antiquote "[" ":" ":]" ;

-- Lambda Abstraction. \x -> x
Abs. Expr ::= "\\" Ident "->" Expr1              ;
-- Application. f x
Ap.  Expr1 ::= Expr1 Expr2                       ;
-- Let variable binding. let x = f x in f x
-- Let. Expr2 ::= "let" Ident "=" Expr2 "in" Expr3  ;
-- Variable. x
Var. Expr3 ::= Ident                             ;
-- Integer literals. 1
Lit. Expr3 ::= Integer                           ;

coercions Expr 3; 

entrypoints Expr;

|]

instance IsString Ident where
   fromString = Ident

type Sym = Ident

-- The Type Sort
data Type 
   = Type :-> Type
   -- ^ Function types
   | TVar Sym
   -- ^ A polymorphic type variable
   | TInt
   -- ^ Integer primitive
   deriving (Show, Eq)
   
makePrisms ''Type
   
data AnnotatedExpr a 
   = AEAbs a Sym (AnnotatedExpr a)
   | AEAp  a     (AnnotatedExpr a) (AnnotatedExpr a)
--   | AELet a     (AnnotatedExpr a)
   | AEVar a Sym
   | AELit a Integer
   deriving (Show, Eq)

makePrisms ''AnnotatedExpr

annotation :: Getter (AnnotatedExpr a) a
annotation f = \case
   AEAbs x y z -> (\a -> AEAbs a y z) <$> f x
   AEAp  x y z -> (\a -> AEAp  a y z) <$> f x
   AEVar x y   -> (\a -> AEVar a y)   <$> f x
   AELit x y   -> (\a -> AELit a y)   <$> f x

type TypedExpr = AnnotatedExpr Type

eraseAnnotation :: AnnotatedExpr a -> Expr
eraseAnnotation = go where
   go = \case 
      AEAbs _ n e -> Abs n $ go e
      AEAp  _ x y -> Ap  (go x)  (go y)
      AEVar _ n   -> Var n
      AELit _ i   -> Lit i

data TypeError 
   = Mismatch 
   | FailedToUnify Type Type
   | OccursCheck Sym Type
   | LookupFailed Sym
   | UnknownError String
   deriving (Show, Eq)

makePrisms ''TypeError

instance Error TypeError where 
   strMsg = UnknownError

newtype Subst = Subst { unSubst :: Map Sym Type }
   deriving (Show, Eq)
   
subst :: Iso' Subst (Map Sym Type)
subst = iso unSubst Subst

-- I'm not sure if this is correct 

instance Monoid Subst where
   mempty  = Subst M.empty
   Subst x `mappend` Subst y = Subst 
      $ M.map (sub (Subst x)) y `M.union` x

lookupS :: Sym -> Subst -> Maybe Type
lookupS x = M.lookup x . unSubst

unit :: Sym -> Type -> Subst
unit s = Subst . M.singleton s

type TyVar = Sym 

class HasTyVars t where
  tyVars :: Traversal' t TyVar
  
instance HasTyVars Type where
   tyVars f = go where
      go = \case
         a :-> b -> (:->) <$> go a <*> go b
         TVar x  -> TVar  <$> f x
         TInt    -> pure TInt
         
instance HasTyVars Subst where
   tyVars = subst . each . tyVars 

freeTyVars :: HasTyVars a => a -> Set TyVar
freeTyVars = setOf tyVars

class Subsitute a where
   sub :: Subst -> a -> a
   
instance Subsitute Type where
   sub s = \case 
      a :-> b -> sub s a :-> sub s b
      TVar x  -> fromMaybe (TVar x) $ lookupS x s
      TInt    -> TInt
      
instance Subsitute Subst where
   sub s = over (subst . each) (sub s)  

boundVars :: Subst -> Set TyVar
boundVars = S.fromList . M.keys . unSubst



