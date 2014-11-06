module Language.SimplePoly.Unification where
import Language.SimplePoly.Types
import Control.Monad.Error  hiding (lift)
import Control.Lens
import Control.Lens.TH
import Data.Monoid
import qualified Data.Set as S

mgu :: Type -> Type -> Either TypeError Subst
mgu x y = case (x, y) of
   (ix :-> ox, iy :-> oy) -> do
      si <- mgu ix iy
      so <- mgu (sub si ox) (sub si oy)
      return $ si <> so
   (TVar n, a     ) -> varBind n a
   (a     , TVar n) -> varBind n a
   (TInt  , TInt  ) -> Right mempty
   _                -> Left $ FailedToUnify x y

-- Check for occurs. If not add to the subs   
varBind :: Sym -> Type -> Either TypeError Subst
varBind n ty 
   | ty == TVar n               = Right mempty
   | n `S.member` freeTyVars ty = Left $ OccursCheck n ty
   | otherwise                  = Right $ unit n ty
