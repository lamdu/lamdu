{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, TemplateHaskell #-}

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

import qualified Control.Lens as Lens
import           Data.Binary (Binary(..))
import           Data.Foldable (traverse_)
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.Store.Rev.Change (Change)
import qualified Data.Store.Rev.Change as Change
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           GHC.Generics (Generic)

-- This key is XOR'd with object keys to yield the IRef to each
-- object's current version ref:
newtype View m = View (IRef m (ViewData m))
    deriving (Eq, Ord, Binary, Show, Read)

data BranchData m = BranchData
    { _brVersion :: Version m
    , _brViews :: [View m]
    } deriving (Eq, Ord, Read, Show, Generic)
instance Binary (BranchData m)

newtype Branch m = Branch { unBranch :: IRef m (BranchData m) }
    deriving (Eq, Ord, Read, Show, Binary)

newtype ViewData m = ViewData { _vdBranch :: Branch m }
    deriving (Eq, Ord, Show, Read, Binary)

Lens.makeLenses ''BranchData
Lens.makeLenses ''ViewData

-- | moveView must be given the correct source of the movement
-- | or it will result in undefined results!
moveView :: Monad m => View m -> Version m -> Version m -> Transaction m ()
moveView vm =
    Version.walk applyBackward applyForward
    where
        applyForward = apply Change.newValue
        applyBackward = apply Change.oldValue
        apply changeDir = applyChangesToView vm changeDir . Version.changes

makeViewKey :: View m -> Change.Key -> UUID
makeViewKey (View iref) = UUIDUtils.combine . IRef.uuid $ iref

applyChangesToView ::
    Monad m => View m -> (Change -> Maybe Change.Value) ->
    [Change] -> Transaction m ()
applyChangesToView vm changeDir = traverse_ applyChange
    where
        applyChange change =
            setValue
            (makeViewKey vm $ Change.objectKey change)
            (changeDir change)
        setValue key Nothing = Transaction.delete key
        setValue key (Just value) = Transaction.insertBS key value
