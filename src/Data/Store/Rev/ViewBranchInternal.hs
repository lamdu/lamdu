{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}

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

import           Control.Monad          (when)
import qualified Data.Store.Guid        as Guid
import           Data.Store.Guid        (Guid)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Rev.Change  (Change)
import qualified Data.Store.Rev.Change  as Change
import           Data.Binary            (Binary(..))
import           Data.Binary.Utils      (get2, put2)
import           Data.Store.IRef        (IRef)
import qualified Data.Store.IRef        as IRef
import qualified Data.Record.Label      as Label
import           Data.Record.Label      ((:->), mkLabels, lens)

newtype ViewData = ViewData { _vdBranch :: Branch }
  deriving (Binary, Eq, Ord, Show, Read)

-- This key is XOR'd with object keys to yield the IRef to each
-- object's current version ref:
newtype View = View (IRef ViewData)
  deriving (Eq, Ord, Binary, Show, Read)

data BranchData = BranchData {
  _brVersion :: Version,
  _brViews :: [View]
  }
  deriving (Eq, Ord, Read, Show)
newtype Branch = Branch (IRef BranchData)
  deriving (Eq, Ord, Read, Show, Binary)

$(mkLabels [''ViewData, ''BranchData])
-- vdBranch :: ViewData :-> Branch
-- brVersion :: BranchData :-> Version
-- brViews :: BranchData :-> [View]

instance Binary BranchData where
  get = get2 BranchData
  put (BranchData a b) = put2 a b

-- | moveView must be given the correct source of the movement
-- | or it will result in undefined results!
moveView :: Monad m => View -> Version -> Version -> Transaction t m ()
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
makeViewKey (View iref) = Guid.xor . IRef.guid $ iref

applyChangesToView :: Monad m => View -> Change.Dir -> [Change] -> Transaction t m ()
applyChangesToView vm changeDir = mapM_ applyChange
  where
    applyChange change = setValue
                         (makeViewKey vm $ Label.getL Change.objectKey change)
                         (Label.getL changeDir change)
    setValue key Nothing      = Transaction.deleteBS key
    setValue key (Just value) = Transaction.insertBS key value
