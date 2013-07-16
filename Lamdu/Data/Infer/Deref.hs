module Lamdu.Data.Infer.Deref
  ( Error(..), deref
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Error = InfiniteExpression

deref :: Ref -> StateT (Context def) (Either Error) (Expr.Expression def ())
deref =
  go Set.empty
  where
    go visited ref
      | visited ^. Lens.contains ref = lift $ Left InfiniteExpression
      | otherwise = do
        RefData _ body <- ExprRefs.read ref
        body
          & Lens.traverse %%~ go (visited & Lens.contains ref .~ True)
          <&> ExprUtil.pureExpression
