module Language.SimplePoly.Free where
import Control.Monad.Free
import Control.Comonad.Cofree
import Bound
import TypeCompose

data Type = Type

newtype a :.: b = O (a b)

-- This is no longer a functor
-- So I can't seem to use bound and free monads
-- I wond
data TermF s a 
   = App a a
   | Lam (s :.: a)
   
instance Functor s => Functor (TermF s) where
   fmap f = \case
   
   
--newtype Term a = Term (Free (TermF (Scope () Term a)) a)



