module Lamdu.Data.Infer.ExprRefs
  ( fresh, find
  , readRep, writeRep
  , popRep
  , read, write
  , union, equiv
  , exprIntoContext
  , unifyRefs
  ) where

import Prelude hiding (read)
import Control.Applicative ((<$>), (<$), (<*))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (runWriterT)
import Control.MonadA (MonadA)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Lamdu.Data.Expression.Infer.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (InferT)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer.UnionFind as UF
import qualified Lamdu.Data.Infer.Monad as InferT

state ::
  Monad m =>
  StateT (ExprRefs def) (EitherT (Error def) m) a ->
  InferT def m a
state = InferT.liftContext . Lens.zoom ctxExprRefs

fresh :: MonadA m => RefData def -> InferT def m Ref
fresh dat = do
  rep <- state $ Lens.zoom exprRefsUF UF.freshRef
  writeRep rep dat
  return rep

find ::
  MonadA m => String -> Ref -> InferT def m Ref
find msg = state . Lens.zoom exprRefsUF . UF.lookup msg

readRep ::
  MonadA m => Ref -> InferT def m (RefData def)
readRep rep =
  unsafeUnjust ("missing ref: " ++ show rep) <$>
  (state . Lens.use) (exprRefsData . Lens.at rep)

popRep ::
  MonadA m => Ref -> InferT def m (RefData def)
popRep rep =
  state . Lens.zoom (exprRefsData . Lens.at rep) $
  unsafeUnjust ("missing ref: " ++ show rep)
  <$> State.get <* State.put Nothing

writeRep ::
  Monad m => Ref -> RefData def -> InferT def m ()
writeRep rep dat = state $ exprRefsData . Lens.at rep .= Just dat

read ::
  MonadA m => Ref -> InferT def m (RefData def)
read ref = readRep =<< find "read" ref

write ::
  MonadA m => Ref -> RefData def -> InferT def m ()
write ref dat =
  (`writeRep` dat) =<< find "write" ref

union :: MonadA m => Ref -> Ref -> InferT def m Ref
union x y = state . Lens.zoom exprRefsUF $ UF.union x y

equiv :: MonadA m => Ref -> Ref -> InferT def m Bool
equiv x y = state . Lens.zoom exprRefsUF $ UF.equivalent x y

unifyRefs ::
  MonadA m =>
  (RefData def ->
   RefData def ->
   InferT def m (RefData def)) ->
  Ref -> Ref -> InferT def m Ref
unifyRefs mergeRefData x y = do
  xRep <- find "unify.x" x
  yRep <- find "unify.y" y
  if xRep == yRep
    then return xRep
    else do
      xData <- popRep xRep
      yData <- popRep yRep
      rep <- union x y
      writeRep rep $ error "Attempt to read parent during unification"
      newData <- mergeRefData xData yData
      writeRep rep newData
      return rep

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
      lift $ fresh RefData
        { _rdVars = RefVars scope getVars
        , _rdBody = newBody
        }
