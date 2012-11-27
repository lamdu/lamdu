{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- | View and Branch have a cyclic dependency. This module
-- | contains the parts of both that both may depend on, to avoid the
-- | cycle.
module Data.Store.Rev.ViewBranchInternal
    (ViewData(..), vdBranch,
     View(..),
     BranchData(..), brVersion, brViews,
     Branch(..),
     moveView, applyChangesToView, makeViewKey)
where

import Control.Monad (when)
import Data.Binary (Binary(..))
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
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
newtype View = View (IRef ViewData)
  deriving (Eq, Ord, Binary, Show, Read)

data BranchData = BranchData {
  _brVersion :: Version,
  _brViews :: [View]
  }
  deriving (Eq, Ord, Read, Show)

newtype Branch = Branch { unBranch :: IRef BranchData }
  deriving (Eq, Ord, Read, Show, Binary)

newtype ViewData = ViewData { _vdBranch :: Branch }
  deriving (Binary, Eq, Ord, Show, Read)

derive makeBinary ''BranchData
LensTH.makeLenses ''BranchData
LensTH.makeLenses ''ViewData

-- | moveView must be given the correct source of the movement
-- | or it will result in undefined results!
moveView :: Monad m => View -> Version -> Version -> Transaction m ()
moveView vm srcVersion destVersion =
  when (srcVersion /= destVersion) $ do
    mraIRef <- Version.mostRecentAncestor srcVersion destVersion
    Version.walkUp applyBackward mraIRef srcVersion
    Version.walkDown applyForward mraIRef destVersion
  where
    applyForward = apply Change.newValue
    applyBackward = apply Change.oldValue
    apply changeDir version = applyChangesToView vm changeDir . Version.changes $ version

makeViewKey :: View -> Change.Key -> Guid
makeViewKey (View iref) = Guid.combine . IRef.guid $ iref

applyChangesToView ::
  Monad m => View -> (Change -> Maybe Change.Value) ->
  [Change] -> Transaction m ()
applyChangesToView vm changeDir = mapM_ applyChange
  where
    applyChange change = setValue
                         (makeViewKey vm $ Change.objectKey change)
                         (changeDir change)
    setValue key Nothing      = Transaction.delete key
    setValue key (Just value) = Transaction.insertBS key value
