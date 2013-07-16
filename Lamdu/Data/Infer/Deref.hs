{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Deref
  ( Error(..), deref, derefExpr
  , Derefed(..), dValue, dType
  ) where

import Control.Applicative (Applicative(..), (<$>))
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

data Derefed def = Derefed
  { _dValue :: Expr.Expression def ()
  , _dType :: Expr.Expression def ()
  }
Lens.makeLenses ''Derefed

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

derefExpr ::
  Expr.Expression defa (ScopedTypedValue, a) ->
  StateT (Context defb) (Either Error)
  (Expr.Expression defa (Derefed defb, a))
derefExpr expr =
  expr
  & Lens.traverse . Lens._1 %%~ derefEach . (^. stvTV)
  where
    derefEach (TypedValue valRef typeRef) =
      Derefed <$> deref valRef <*> deref typeRef
