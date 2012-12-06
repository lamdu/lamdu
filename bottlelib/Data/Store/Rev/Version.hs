{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.Version
  ( VersionData, depth, parent, changes
  , preventUndo
  , Version, versionIRef, versionData
  , makeInitialVersion, newVersion, mostRecentAncestor
  , walkUp, walkDown, versionsBetween
  ) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Foldable (traverse_)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Change (Change(..), Key, Value)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction

newtype Version = Version { versionIRef :: IRef VersionData }
  deriving (Eq, Ord, Read, Show, Binary)

data VersionData = VersionData {
  depth :: Int,
  parent :: Maybe Version,
  changes :: [Change]
  }
  deriving (Eq, Ord, Read, Show)
$(derive makeBinary ''VersionData)

makeInitialVersion :: MonadA m => [(Key, Value)] -> Transaction m Version
makeInitialVersion initialValues = fmap Version . Transaction.newIRef . VersionData 0 Nothing $ map makeChange initialValues
  where
    makeChange (key, value) = Change key Nothing (Just value)

versionData :: MonadA m => Version -> Transaction m VersionData
versionData = Transaction.readIRef . versionIRef

-- TODO: This is a hack. Used to prevent undo into initial empty
-- version. Can instead explicitly make a version when running a
-- "view" transaction
preventUndo :: MonadA m => Version -> Transaction m ()
preventUndo version = do
  ver <- versionData version
  Transaction.writeIRef (versionIRef version)
    ver { parent = Nothing }

newVersion :: MonadA m => Version -> [Change] -> Transaction m Version
newVersion version newChanges = do
  parentDepth <- fmap depth . versionData $ version
  fmap Version .
    Transaction.newIRef .
    VersionData (parentDepth+1) (Just version) $
    newChanges

mostRecentAncestor :: MonadA m => Version -> Version -> Transaction m Version
mostRecentAncestor aVersion bVersion
  | aVersion == bVersion  = return aVersion
  | otherwise             = do
    VersionData aDepth aMbParentRef _aChanges <- versionData aVersion
    VersionData bDepth bMbParentRef _bChanges <- versionData bVersion
    case compare aDepth bDepth of
      LT -> (aVersion `mostRecentAncestor`) =<< upToDepth aDepth bVersion
      GT -> (`mostRecentAncestor` bVersion) =<< upToDepth bDepth aVersion
      EQ -> if aDepth == 0
            then fail "Two versions without common ancestor given"
            else join $ liftA2 mostRecentAncestor (getParent aMbParentRef) (getParent bMbParentRef)
  where
    upToDepth depthToReach version = do
      VersionData curDepth curMbParentRef _curChanges <- versionData version
      if curDepth > depthToReach
        then upToDepth depthToReach =<< getParent curMbParentRef
        else return version
    getParent = maybe (fail "Non-0 depth must have a parent") return

walkUp :: MonadA m => (VersionData -> Transaction m ()) -> Version -> Version -> Transaction m ()
walkUp onVersion topRef bottomRef
  | bottomRef == topRef  = return ()
  | otherwise            = do
    versionD <- versionData bottomRef
    onVersion versionD
    maybe (fail "Invalid path given, hit top") (walkUp onVersion topRef) $
      parent versionD

-- We can't directly walkDown (we don't have references pointing
-- downwards... But we can generate a list of versions by walking up
-- and accumulating a reverse list)
versionsBetween :: MonadA m => Version -> Version -> Transaction m [VersionData]
versionsBetween topRef = accumulateWalkUp []
  where
    accumulateWalkUp vs curRef
      | topRef == curRef  = return vs
      | otherwise         = do
        versionD <- versionData curRef
        maybe (fail "Invalid path given, hit top") (accumulateWalkUp (versionD:vs)) $
          parent versionD

-- Implement in terms of versionsBetween
walkDown :: MonadA m => (VersionData -> Transaction m ()) -> Version -> Version -> Transaction m ()
walkDown onVersion topRef bottomRef =
  traverse_ onVersion =<< versionsBetween topRef bottomRef
