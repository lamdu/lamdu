module Lamdu.Data.Infer.ExprRefs
  ( fresh, freshHole, find
  , readRep, writeRep
  , popRep
  , read, write
  , union, equiv
  , exprIntoContext
  , unifyRefs
  ) where

import Control.Applicative ((<$>), (<*))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Prelude hiding (read)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens

fresh :: MonadA m => RefData def -> StateT (Context def) m Ref
fresh dat = do
  rep <- Lens.zoom (ctxExprRefs . exprRefsUF) UF.freshRef
  writeRep rep dat
  return rep

freshHole :: MonadA m => StateT (Context def) m Ref
freshHole = fresh . RefData (Scope Map.empty) $ ExprLens.bodyHole # ()

find :: MonadA m => String -> Ref -> StateT (Context def) m Ref
find msg = Lens.zoom (ctxExprRefs . exprRefsUF) . UF.lookup msg

readRep ::
  MonadA m => Ref -> StateT (Context def) m (RefData def)
readRep rep =
  unsafeUnjust ("missing ref: " ++ show rep) <$>
  (Lens.zoom ctxExprRefs . Lens.use) (exprRefsData . Lens.at rep)

popRep ::
  MonadA m => Ref -> StateT (Context def) m (RefData def)
popRep rep =
  Lens.zoom (ctxExprRefs . exprRefsData . Lens.at rep) $
  unsafeUnjust ("missing ref: " ++ show rep)
  <$> State.get <* State.put Nothing

writeRep ::
  Monad m => Ref -> RefData def -> StateT (Context def) m ()
writeRep rep dat = Lens.zoom ctxExprRefs $ exprRefsData . Lens.at rep .= Just dat

read ::
  MonadA m => Ref -> StateT (Context def) m (RefData def)
read ref = readRep =<< find "read" ref

write ::
  MonadA m => Ref -> RefData def -> StateT (Context def) m ()
write ref dat =
  (`writeRep` dat) =<< find "write" ref

union :: MonadA m => Ref -> Ref -> StateT (Context def) m Ref
union x y = Lens.zoom (ctxExprRefs . exprRefsUF) $ UF.union x y

equiv :: MonadA m => Ref -> Ref -> StateT (Context def) m Bool
equiv x y = Lens.zoom (ctxExprRefs . exprRefsUF) $ UF.equivalent x y

unifyRefs ::
  MonadA m =>
  (Ref ->
   RefData def ->
   RefData def ->
   StateT (Context def) m (RefData def, res)) ->
  Ref -> Ref -> StateT (Context def) m (Maybe res)
unifyRefs mergeRefData x y = do
  xRep <- find "unify.x" x
  yRep <- find "unify.y" y
  if xRep == yRep
    then return Nothing
    else do
      xData <- popRep xRep
      yData <- popRep yRep
      rep <- union x y
      writeRep rep $ error "Attempt to read parent during unification"
      (newData, res) <- mergeRefData rep xData yData
      writeRep rep newData
      return $ Just res

exprIntoContext ::
  MonadA m => Expr.Expression def () -> StateT (Context def) m Ref
exprIntoContext =
  go mempty
  where
    go scope (Expr.Expression body ()) = do
      newBody <-
        case body of
        Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
          paramTypeRef <- go scope paramType
          Expr.BodyLam . Expr.Lam k paramGuid paramTypeRef <$>
            go (scope & Lens.at paramGuid .~ Just paramTypeRef) result
        -- Expensive assertion:
        Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid))
          | Lens.has Lens._Nothing (scope ^. Lens.at guid) -> error "GetVar out of scope"
        _ -> body & Lens.traverse %%~ go scope
      fresh RefData
        { _rdScope = Scope scope
        , _rdBody = newBody
        }
