{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Deref
  ( deref, expr
  , Derefed(..), dValue, dType
  , Error(..)
  , Restrictions(..)
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Function.Decycle (decycle)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.RefTags (ExprRef)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr

data Error def = InfiniteExpression (ExprRef def)
  deriving (Show, Eq, Ord)

-- Some infer info flows are one-way. In that case, we may know that
-- an expression must match certain other expressions either directly
-- or via use of a getvar from the src's context. These are documented
-- in restrictions:
newtype Restrictions def = Restrictions
  { _rRefs :: [ExprRef def]
  }

data Derefed def = Derefed
  { _dValue :: Expr.Expression def (Restrictions def)
  , _dType :: Expr.Expression def (Restrictions def)
  }
Lens.makeLenses ''Derefed

deref :: ExprRef def -> StateT (Context def) (Either (Error def)) (Expr.Expression def (Restrictions def))
deref =
  Lens.zoom ctxUFExprs . (`evalStateT` Map.empty) . decycle loop
  where
    loop Nothing ref = lift . lift . Left $ InfiniteExpression ref
    loop (Just recurse) ref = do
      rep <- lift $ UFData.find "deref lookup" ref
      mFound <- Lens.use (Lens.at rep)
      case mFound of
        Just found -> return found
        Nothing -> do
          repData <- lift $ State.gets (UFData.readRep rep)
          let
            body = repData ^. rdBody
            restrictions =
              Restrictions $
              case body of
              Expr.BodyLeaf Expr.Hole ->
                [ apr ^. aprDestRef
                | RelationAppliedPiResult apr <- repData ^. rdRelations
                ]
              _ -> [] -- Only holes need restrictions
          derefed <-
            repData ^. rdBody
            & Lens.traverse %%~ recurse
            <&> (`Expr.Expression` restrictions)
          Lens.at rep .= Just derefed
          return derefed

expr ::
  Expr.Expression defa (ScopedTypedValue defb, a) ->
  StateT (Context defb) (Either (Error defb)) (Expr.Expression defa (Derefed defb, a))
expr =
  Lens.traverse . Lens._1 %%~ derefEach . (^. stvTV)
  where
    derefEach (TypedValue valRef typeRef) =
      Derefed <$> deref valRef <*> deref typeRef
