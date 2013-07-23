module Lamdu.Data.Infer.Optimize
  ( optimizeContext
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), execStateT, evalState)
import Control.Monad.Trans.Writer (runWriter, execWriter)
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntMap as IntMap
import qualified Data.UnionFind as UF

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
      <&> error "TODO: Also fix rule refs!"
  ctxDefTVs . Lens.mapped . tvRefs %= refRename "defRefs"
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
