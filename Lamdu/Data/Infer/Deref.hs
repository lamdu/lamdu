{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Deref
  ( deref, expr
  , Derefed(..), dValue, dType
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function.Decycle (decycle)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Derefed def = Derefed
  { _dValue :: Expr.Expression def ()
  , _dType :: Expr.Expression def ()
  }
derive makeBinary ''Derefed
Lens.makeLenses ''Derefed

deref :: MonadA m => Ref -> StateT (Context def) m (Expr.Expression def ())
deref =
  (`evalStateT` Map.empty) . decycle loop
  where
    loop Nothing _ = error "Cycle at deref?!"
    loop (Just recurse) ref = do
      rep <- lift $ ExprRefs.find "deref lookup" ref
      mFound <- Lens.use (Lens.at rep)
      case mFound of
        Just found -> return found
        Nothing -> do
          derefed <-
            ExprRefs.readRep rep
            & lift
            <&> (^. rdBody)
            >>= Lens.traverse %%~ recurse
            <&> ExprUtil.pureExpression
          Lens.at rep .= Just derefed
          return derefed

expr ::
  MonadA m =>
  Expr.Expression defa (ScopedTypedValue, a) ->
  StateT (Context defb) m (Expr.Expression defa (Derefed defb, a))
expr =
  Lens.traverse . Lens._1 %%~ derefEach . (^. stvTV)
  where
    derefEach (TypedValue valRef typeRef) =
      Derefed <$> deref valRef <*> deref typeRef
