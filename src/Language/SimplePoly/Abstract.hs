{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.SimplePoly.Abstract where
import Language.SimplePoly.LensSyntax
import Language.SimplePoly.Scope
import Bound
import Data.Foldable
import Data.Traversable
import Prelude.Extras
import Data.Data
import GHC.Generics
import Control.Lens
import Control.Comonad.Cofree

-------------------------------------------------------------------------------
--                                  Kinds
-------------------------------------------------------------------------------
-- The Kind Sort
data Kind a 
   = KArr !(Kind a) !(Kind a)
   | Star
   deriving (Show, Eq, Functor, Foldable, Traversable, Typeable, Data, Generic)

makePrisms ''Kind

instance Show1 Kind
instance Eq1   Kind
-------------------------------------------------------------------------------
--                                  Types
-------------------------------------------------------------------------------

-- The Type Sort
data Type v
   = TVar v
   -- ^ A polymorphic type variable
   | TArr !(Type v) !(Type v)
   -- ^ Function types
   | TInt 
   -- ^ Integer primitive
   deriving (Show, Eq, Functor, Foldable, Traversable, Typeable, Data, Generic)

makePrisms ''Type

instance Monad Type where
   return = TVar
   m >>= f = case m of
      TVar    x   -> f x
      TArr    g x -> TArr (g >>= f) (x >>= f)
      TInt        -> TInt 
                          
instance Show1 Type
instance Eq1   Type


-- Prisms
-- 

-------------------------------------------------------------------------------
--                                  Terms
-------------------------------------------------------------------------------

data Term v 
   = Var v  
   | App !(Term v) !(Term v)
   | Lam !(Scope () Term v)
   | Lit Int
   deriving (Eq, Show, Functor, Foldable, Traversable, Typeable, Data, Generic)

instance Show1 Term
instance Eq1   Term

instance Monad Term where
   return = Var
   m >>= f = case m of
      Var     x   -> f x
      App     h x -> App (h >>=  f) (x >>= f)
      Lam     b   -> Lam (b >>>= f)
      Lit     x   -> Lit x
                         
makePrisms ''Term

















