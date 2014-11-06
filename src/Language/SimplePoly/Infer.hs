{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.SimplePoly.Infer where
import Language.SimplePoly.Types
import Language.SimplePoly.Unification
import Control.Lens
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Arrow
import Control.Applicative
import Data.Monoid
import qualified Data.Set as S
import qualified Control.Monad.State as S

data InferenceState = InferenceState 
   { _supply :: Int
   , _subs   :: Subst
   }
   deriving(Show, Eq)

instance Monoid InferenceState where
   mempty = InferenceState 0 mempty
   InferenceState xSupply xSubs `mappend` InferenceState ySupply ySubs = 
      InferenceState (xSupply + ySupply) (xSubs <> ySubs)

makeLenses ''InferenceState

type Env m = StateT InferenceState (ErrorT TypeError m)

runEnv :: Env Identity Type -> Either TypeError Type
runEnv = runEnvWith mempty

runEnvWith :: InferenceState 
           -> Env Identity Type 
           -> Either TypeError Type
runEnvWith initial = runIdentity . runEnvWithT initial  

runEnvT :: (Functor m, Monad m) => Env m Type -> m (Either TypeError Type)
runEnvT = runEnvWithT mempty
   
runEnvWithT :: (Functor m, Monad m) 
           => InferenceState 
           -> Env m Type 
           -> m (Either TypeError Type)
runEnvWithT initial action = runErrorT $ evalStateT action initial 

extendSubst u = subs <>= u

extendVars n e = subs <>= unit n e

eitherToError :: (Error e, Monad m, MonadError e m) => Either e a -> m a
eitherToError = either throwError return

unify :: Monad m => Type -> Type -> Env m ()
unify t1 t2 = do 
   s <- use subs
   u <- eitherToError $ mgu (sub s t1) (sub s t2)
   extendSubst u

lookupType :: Monad m => Sym -> Env m Type
lookupType n =   (S.lift . ErrorT . return . maybe (Left (LookupFailed n)) Right) 
             =<< use (subs . to (lookupS n))

inc :: MonadState InferenceState m => m Int
inc = do 
  supply += 1 
  use supply

newVar :: Monad m => Env m Type
newVar = do
   i <- inc
   return . TVar . Ident $ "a" ++ show i

-- I need to implement capture avoiding subsitution
infer :: Expr -> Either TypeError Type
infer = runEnv . infer'

inferT :: (Functor m, Monad m) => Expr -> m (Either TypeError Type)
inferT = runEnvT . infer'

infer' :: Monad m => Expr -> Env m Type
infer' = \case
   Lit _   -> return TInt
   Var n   -> lookupType n
   Ap f x  -> do
        ftyp    <- infer' f
        argType <- infer' x
        output@(TVar n)  <- newVar
        extendVars n output
        unify ftyp $ argType :-> output
        lookupType n 

   Abs n e -> do
      extendVars n =<< newVar
      output <- infer' e
      input  <- lookupType n
      return $ input :-> output

--   Let n e0 e1 -> do
--      extendVars n =<< infer' e0
--      infer' e1
