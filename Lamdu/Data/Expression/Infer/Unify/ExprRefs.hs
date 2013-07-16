module Lamdu.Data.Expression.Infer.Unify.ExprRefs
  ( exprRefsUF, exprRefsData
  , exprRefsFresh, exprRefsFind
  , exprRefsReadRep, exprRefsWriteRep
  , exprRefsPopRep
  , exprRefsRead, exprRefsWrite
  , exprRefsUnion, exprRefsEquiv
  , exprIntoContext
  ) where

import Control.Applicative ((<$>), (<$), (<*))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (runWriterT)
import Control.MonadA (MonadA)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Lamdu.Data.Expression.Infer.Unify.Internal
import Lamdu.Data.Expression.Infer.Unify.Monad (InferT)
import Lamdu.Data.Expression.Infer.UnionFind (Ref)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer.Unify.Monad as InferT
import qualified Lamdu.Data.Expression.Infer.UnionFind as UF

exprRefsState ::
  Monad m =>
  StateT (ExprRefs def) (EitherT (Error def) m) a ->
  InferT def m a
exprRefsState = InferT.liftContext . Lens.zoom ctxExprRefs

exprRefsFresh :: MonadA m => RefData def -> InferT def m Ref
exprRefsFresh dat = do
  rep <- exprRefsState $ Lens.zoom exprRefsUF UF.freshRef
  exprRefsWriteRep rep dat
  return rep

exprRefsFind ::
  MonadA m => String -> Ref -> InferT def m Ref
exprRefsFind msg = exprRefsState . Lens.zoom exprRefsUF . UF.lookup msg

exprRefsReadRep ::
  MonadA m => Ref -> InferT def m (RefData def)
exprRefsReadRep rep =
  unsafeUnjust ("missing ref: " ++ show rep) <$>
  (exprRefsState . Lens.use) (exprRefsData . Lens.at rep)

exprRefsPopRep ::
  MonadA m => Ref -> InferT def m (RefData def)
exprRefsPopRep rep =
  exprRefsState . Lens.zoom (exprRefsData . Lens.at rep) $
  unsafeUnjust ("missing ref: " ++ show rep)
  <$> State.get <* State.put Nothing

exprRefsWriteRep ::
  Monad m => Ref -> RefData def -> InferT def m ()
exprRefsWriteRep rep dat = exprRefsState $ exprRefsData . Lens.at rep .= Just dat

exprRefsRead ::
  MonadA m => Ref -> InferT def m (RefData def)
exprRefsRead ref = exprRefsReadRep =<< exprRefsFind "exprRefsRead" ref

exprRefsWrite ::
  MonadA m => Ref -> RefData def -> InferT def m ()
exprRefsWrite ref dat =
  (`exprRefsWriteRep` dat) =<< exprRefsFind "exprRefsWrite" ref

exprRefsUnion :: MonadA m => Ref -> Ref -> InferT def m Ref
exprRefsUnion x y = exprRefsState . Lens.zoom exprRefsUF $ UF.union x y

exprRefsEquiv :: MonadA m => Ref -> Ref -> InferT def m Bool
exprRefsEquiv x y = exprRefsState . Lens.zoom exprRefsUF $ UF.equivalent x y

exprIntoContext ::
  MonadA m => Expr.Expression def () -> InferT def m Ref
exprIntoContext =
  fmap verifyEmptyGetVars . runWriterT . go mempty
  where
    -- Expensive assertion
    verifyEmptyGetVars (ref, getVars)
      | Set.null getVars = ref
      | otherwise = error "GetVar not in Lam"
    go scope (Expr.Expression body ()) = do
      (newBody, getVars) <-
        Writer.listen $
        case body of
        Expr.BodyLam (Expr.Lam k paramGuid paramType result) ->
          Writer.censor (Set.delete paramGuid) $ do
            paramTypeRef <- go scope paramType
            Expr.BodyLam . Expr.Lam k paramGuid paramTypeRef <$>
              go (scope & Lens.at paramGuid .~ Just paramTypeRef) result
        Expr.BodyLeaf leaf@(Expr.GetVariable (Expr.ParameterRef guid)) ->
          Expr.BodyLeaf leaf <$ Writer.tell (Set.singleton guid)
        _ -> body & Lens.traverse %%~ go scope
      lift $ exprRefsFresh RefData
        { _rdVars = RefVars scope getVars
        , _rdBody = newBody
        }
