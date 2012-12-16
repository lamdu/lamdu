{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- | View and Branch have a cyclic dependency. This module
-- | contains the parts of both that both may depend on, to avoid the
-- | cycle.
module Data.Store.Rev.ViewBranchInternal
  ( ViewData(..), vdBranch
  , View(..)
  , BranchData(..), brVersion, brViews
  , Branch(..)
  , moveView, applyChangesToView, makeViewKey
  )
where

import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Rev.Change (Change)
import Data.Store.Rev.Version (Version)
import Data.Store.Transaction (Transaction)
import qualified Control.Lens.TH as LensTH
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Transaction as Transaction

-- This key is XOR'd with object keys to yield the IRef to each
-- object's current version ref:
newtype View t = View (IRef t (ViewData t))
  deriving (Eq, Ord, Binary, Show, Read)

data BranchData t = BranchData {
  _brVersion :: Version t,
  _brViews :: [View t]
  } deriving (Eq, Ord, Read, Show)

newtype Branch t = Branch { unBranch :: IRef t (BranchData t) }
  deriving (Eq, Ord, Read, Show, Binary)

newtype ViewData t = ViewData { _vdBranch :: Branch t }
  deriving (Eq, Ord, Show, Read, Binary)

derive makeBinary ''BranchData
LensTH.makeLenses ''BranchData
LensTH.makeLenses ''ViewData

-- | moveView must be given the correct source of the movement
-- | or it will result in undefined results!
moveView :: MonadA m => View (Tag m) -> Version (Tag m) -> Version (Tag m) -> Transaction m ()
moveView vm srcVersion destVersion =
  when (srcVersion /= destVersion) $ do
    mraIRef <- Version.mostRecentAncestor srcVersion destVersion
    Version.walkUp applyBackward mraIRef srcVersion
    Version.walkDown applyForward mraIRef destVersion
  where
    applyForward = apply Change.newValue
    applyBackward = apply Change.oldValue
    apply changeDir version = applyChangesToView vm changeDir . Version.changes $ version

makeViewKey :: View t -> Change.Key -> Guid
makeViewKey (View iref) = Guid.combine . IRef.guid $ iref

applyChangesToView ::
  MonadA m => View (Tag m) -> (Change -> Maybe Change.Value) ->
  [Change] -> Transaction m ()
applyChangesToView vm changeDir = traverse_ applyChange
  where
    applyChange change = setValue
                         (makeViewKey vm $ Change.objectKey change)
                         (changeDir change)
    setValue key Nothing      = Transaction.delete key
    setValue key (Just value) = Transaction.insertBS key value
