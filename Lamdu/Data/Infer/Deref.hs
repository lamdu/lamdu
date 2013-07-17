{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Deref
  ( Error(..), deref, expr
  , Derefed(..), dValue, dType
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Data.Function.Decycle (decycle)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Error = InfiniteExpression
  deriving (Show)

data Derefed def = Derefed
  { _dValue :: Expr.Expression def ()
  , _dType :: Expr.Expression def ()
  }
Lens.makeLenses ''Derefed

deref :: Ref -> StateT (Context def) (Either Error) (Expr.Expression def ())
deref =
  decycle (lift (Left InfiniteExpression)) loop
  where
    loop recurse ref = do
      RefData _ body <- ExprRefs.read ref
      body
        & Lens.traverse %%~ recurse
        <&> ExprUtil.pureExpression

expr ::
  Expr.Expression defa (ScopedTypedValue, a) ->
  StateT (Context defb) (Either Error)
  (Expr.Expression defa (Derefed defb, a))
expr =
  Lens.traverse . Lens._1 %%~ derefEach . (^. stvTV)
  where
    derefEach (TypedValue valRef typeRef) =
      Derefed <$> deref valRef <*> deref typeRef
