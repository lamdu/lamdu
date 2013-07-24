{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Lamdu.Data.Infer.ExprRefs
  ( ExprRefs, empty
  , fresh, find
  , readRep, writeRep
  , popRep
  , read, write, modify
  , union, equiv
  , UnifyRefsResult(..)
  , unifyRefs
  , optimize
  ) where

import Control.Applicative ((<$>), (<*))
import Control.Arrow ((***))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), execStateT, evalState)
import Control.Monad.Trans.Writer (runWriter)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.UnionFind (Ref, RefMap)
import Prelude hiding (read)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntMap as IntMap
import qualified Data.UnionFind as UF

data ExprRefs a = ExprRefs
  { _exprRefsUF :: UF.UnionFind
  , _exprRefsData :: RefMap a
  } deriving (Functor)
Lens.makeLenses ''ExprRefs

empty :: ExprRefs a
empty = ExprRefs
  { _exprRefsUF = UF.empty
  , _exprRefsData = IntMap.empty
  }

fresh :: MonadA m => a -> StateT (ExprRefs a) m Ref
fresh dat = do
  rep <- Lens.zoom exprRefsUF UF.freshRef
  writeRep rep dat
  return rep

find :: MonadA m => String -> Ref -> StateT (ExprRefs a) m Ref
find msg = Lens.zoom exprRefsUF . UF.lookup msg

readRep ::
  MonadA m => Ref -> StateT (ExprRefs a) m a
readRep rep =
  unsafeUnjust ("missing ref: " ++ show rep) <$>
  Lens.use (exprRefsData . Lens.at rep)

popRep ::
  MonadA m => Ref -> StateT (ExprRefs a) m a
popRep rep =
  Lens.zoom (exprRefsData . Lens.at rep) $
  unsafeUnjust ("missing ref: " ++ show rep)
  <$> State.get <* State.put Nothing

writeRep ::
  Monad m => Ref -> a -> StateT (ExprRefs a) m ()
writeRep rep dat = exprRefsData . Lens.at rep .= Just dat

read ::
  MonadA m => Ref -> StateT (ExprRefs a) m a
read ref = readRep =<< find "read" ref

write ::
  MonadA m => Ref -> a -> StateT (ExprRefs a) m ()
write ref dat =
  (`writeRep` dat) =<< find "write" ref

modify ::
  MonadA m => Ref -> (a -> a) ->
  StateT (ExprRefs a) m ()
modify ref f = write ref . f =<< read ref

union :: MonadA m => Ref -> Ref -> StateT (ExprRefs a) m Ref
union x y = Lens.zoom exprRefsUF $ UF.union x y

equiv :: MonadA m => Ref -> Ref -> StateT (ExprRefs a) m Bool
equiv x y = Lens.zoom exprRefsUF $ UF.equivalent x y

data UnifyRefsResult a
  = UnifyRefsAlreadyUnified
  | UnifyRefsUnified a a

unifyRefs ::
  MonadA m => Ref -> Ref ->
  StateT (ExprRefs a) m (Ref, UnifyRefsResult a)
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

optimize ::
  ((Ref -> Ref) -> a -> b) -> ExprRefs a -> (Ref -> Ref, ExprRefs b)
optimize onData (ExprRefs oldUf oldRefsData) =
  ( refRename "ExprRefs.optimize:user ref inexistent"
  , ExprRefs newUf newRefsData
  )
  where
    (newUf, refRenames) =
      runWriter . (`execStateT` UF.empty) $
      oldRefsData & IntMap.keys & traverse_ %%~ freshRef
    refRename msg oldRef =
      let oldRep = (`evalState` oldUf) $ UF.lookup "optimize:in old UF" oldRef
      in refRenames ^? Lens.ix oldRep & unsafeUnjust msg
    newRefsData =
      oldRefsData
      & IntMap.toList
      & map (refRename "optimize:onOldRefItem" ***
             onData (refRename "optimize:onRefData"))
      & IntMap.fromList
    freshRef oldRep = do
      newRep <- UF.freshRef
      lift $ Writer.tell (IntMap.singleton oldRep newRep)
