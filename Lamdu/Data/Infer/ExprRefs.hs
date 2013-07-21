module Lamdu.Data.Infer.ExprRefs
  ( fresh, find
  , readRep, writeRep
  , popRep
  , read, write, modify
  , union, equiv
  , exprIntoContext
  , UnifyRefsResult(..)
  , unifyRefs
  , optimizeContext
  ) where

import Control.Applicative ((<$>), (<*))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), execStateT, evalState)
import Control.Monad.Trans.Writer (runWriter, execWriter)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Prelude hiding (read)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntMap as IntMap
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr

optimizeContext :: Monad m => StateT (Context def) m (Ref -> Ref)
optimizeContext = do
  oldUf <- Lens.use (ctxExprRefs . exprRefsUF)
  oldRefsData <- Lens.use (ctxExprRefs . exprRefsData)
  let
    (newUf, refRenames) =
      runWriter . (`execStateT` UF.empty) $
      oldRefsData & IntMap.keys & traverse_ %%~ freshRef
    refRename msg oldRef =
      let oldRep = (`evalState` oldUf) $ UF.lookup "optimize:in old UF" oldRef
      in refRenames ^? Lens.ix oldRep & unsafeUnjust msg

    onOldRefItem (oldRep, oldRefData) =
      Writer.tell $
      IntMap.singleton (refRename "optimize:onOldRefItem" oldRep)
      (oldRefData & rdRefs %~ refRename "optimize:onRefData")
    newRefsData =
      execWriter $
      oldRefsData & IntMap.toList & traverse_ %%~ onOldRefItem
  ctxDefRefs . Lens.mapped %= refRename "defRefs"
  Lens.zoom ctxExprRefs $
    State.put ExprRefs
    { _exprRefsUF = newUf
    , _exprRefsData = newRefsData
    }
  return $ refRename "optimize:user ref inexistent"
  where
    freshRef oldRep = do
      newRep <- UF.freshRef
      lift $ Writer.tell (IntMap.singleton oldRep newRep)

fresh :: MonadA m => RefData def -> StateT (Context def) m Ref
fresh dat = do
  rep <- Lens.zoom (ctxExprRefs . exprRefsUF) UF.freshRef
  writeRep rep dat
  return rep

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

modify ::
  MonadA m => Ref -> (RefData def -> RefData def) ->
  StateT (Context def) m ()
modify ref f = write ref . f =<< read ref

union :: MonadA m => Ref -> Ref -> StateT (Context def) m Ref
union x y = Lens.zoom (ctxExprRefs . exprRefsUF) $ UF.union x y

equiv :: MonadA m => Ref -> Ref -> StateT (Context def) m Bool
equiv x y = Lens.zoom (ctxExprRefs . exprRefsUF) $ UF.equivalent x y

data UnifyRefsResult def
  = UnifyRefsAlreadyUnified
  | UnifyRefsUnified (RefData def) (RefData def)

unifyRefs ::
  MonadA m => Ref -> Ref ->
  StateT (Context def) m (Ref, UnifyRefsResult def)
unifyRefs x y = do
  xRep <- find "unify.x" x
  yRep <- find "unify.y" y
  if xRep == yRep
    then return (xRep, UnifyRefsAlreadyUnified)
    else do
      xData <- popRep xRep
      yData <- popRep yRep
      rep <- x `union` y
      writeRep rep $ error "unifyRefs caller read the unified ref data before writing it"
      return (rep, UnifyRefsUnified xData yData)

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
      fresh $ defaultRefData (Scope scope) newBody
