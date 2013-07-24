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
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Error = InfiniteExpression Ref
  deriving (Show, Eq, Ord)

-- Some infer info flows are one-way. In that case, we may know that
-- an expression must match certain other expressions either directly
-- or via use of a getvar from the src's context. These are documented
-- in restrictions:
newtype Restrictions def = Restrictions
  { _rRefs :: [Ref]
  }

data Derefed def = Derefed
  { _dValue :: Expr.Expression def (Restrictions def)
  , _dType :: Expr.Expression def (Restrictions def)
  }
Lens.makeLenses ''Derefed

deref :: Ref -> StateT (Context def) (Either Error) (Expr.Expression def (Restrictions def))
deref =
  Lens.zoom ctxExprRefs . (`evalStateT` Map.empty) . decycle loop
  where
    loop Nothing ref = lift . lift . Left $ InfiniteExpression ref
    loop (Just recurse) ref = do
      rep <- lift $ ExprRefs.find "deref lookup" ref
      mFound <- Lens.use (Lens.at rep)
      case mFound of
        Just found -> return found
        Nothing -> do
          repData <- lift $ ExprRefs.readRep rep
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
  Expr.Expression defa (ScopedTypedValue, a) ->
  StateT (Context defb) (Either Error) (Expr.Expression defa (Derefed defb, a))
expr =
  Lens.traverse . Lens._1 %%~ derefEach . (^. stvTV)
  where
    derefEach (TypedValue valRef typeRef) =
      Derefed <$> deref valRef <*> deref typeRef
