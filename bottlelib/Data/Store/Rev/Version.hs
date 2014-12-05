{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Data.Store.Rev.Version
  ( VersionData, depth, parent, changes
  , preventUndo
  , Version, versionIRef, versionData
  , makeInitialVersion, newVersion, mostRecentAncestor
  , walkUp, walkDown, versionsBetween
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Foldable (traverse_)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Change (Change(..), Key, Value)
import Data.Store.Transaction (Transaction)
import GHC.Generics (Generic)
import qualified Data.Store.Transaction as Transaction

newtype Version m = Version { versionIRef :: IRef m (VersionData m) }
  deriving (Eq, Ord, Read, Show, Binary)

data VersionData m = VersionData
  { depth :: Int
  , parent :: Maybe (Version m)
  , changes :: [Change]
  } deriving (Eq, Ord, Read, Show, Generic)
instance Binary (VersionData m)

makeInitialVersion :: MonadA m => [(Key, Value)] -> Transaction m (Version (m))
makeInitialVersion initialValues = fmap Version . Transaction.newIRef . VersionData 0 Nothing $ map makeChange initialValues
  where
    makeChange (key, value) = Change key Nothing (Just value)

versionData :: MonadA m => Version (m) -> Transaction m (VersionData (m))
versionData = Transaction.readIRef . versionIRef

-- TODO: This is a hack. Used to prevent undo into initial empty
-- version. Can instead explicitly make a version when running a
-- "view" transaction
preventUndo :: MonadA m => Version (m) -> Transaction m ()
preventUndo version = do
  ver <- versionData version
  Transaction.writeIRef (versionIRef version)
    ver { parent = Nothing }

newVersion :: MonadA m => Version (m) -> [Change] -> Transaction m (Version (m))
newVersion version newChanges = do
  parentDepth <- fmap depth . versionData $ version
  fmap Version .
    Transaction.newIRef .
    VersionData (parentDepth+1) (Just version) $
    newChanges

mostRecentAncestor ::
  MonadA m => Version (m) -> Version (m) -> Transaction m (Version (m))
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
            else join $ mostRecentAncestor <$> getParent aMbParentRef <*> getParent bMbParentRef
  where
    upToDepth depthToReach version = do
      VersionData curDepth curMbParentRef _curChanges <- versionData version
      if curDepth > depthToReach
        then upToDepth depthToReach =<< getParent curMbParentRef
        else return version
    getParent = maybe (fail "Non-0 depth must have a parent") return

walkUp ::
  MonadA m =>
  (VersionData (m) -> Transaction m ()) ->
  Version (m) -> Version (m) -> Transaction m ()
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
versionsBetween ::
  MonadA m => Version (m) -> Version (m) ->
  Transaction m [VersionData (m)]
versionsBetween topRef = accumulateWalkUp []
  where
    accumulateWalkUp vs curRef
      | topRef == curRef  = return vs
      | otherwise         = do
        versionD <- versionData curRef
        maybe (fail "Invalid path given, hit top") (accumulateWalkUp (versionD:vs)) $
          parent versionD

-- Implement in terms of versionsBetween
walkDown :: MonadA m => (VersionData (m) -> Transaction m ()) -> Version (m) -> Version (m) -> Transaction m ()
walkDown onVersion topRef bottomRef =
  traverse_ onVersion =<< versionsBetween topRef bottomRef
