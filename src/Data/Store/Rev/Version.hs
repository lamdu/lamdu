{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.Version
    (VersionData, depth, parent, changes,
     Version, versionIRef, versionData,
     makeInitialVersion, newVersion, mostRecentAncestor,
     walkUp, walkDown, versionsBetween)
where

import           Control.Monad          (liftM, liftM2, join)
import           Data.Binary            (Binary(..))
import           Data.Store.IRef        (IRef)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Store.Rev.Change  (Change(..), Key, Value)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)

newtype Version = Version { versionIRef :: IRef VersionData }
  deriving (Eq, Ord, Read, Show, Binary)

data VersionData = VersionData {
  depth :: Int,
  parent :: Maybe Version,
  changes :: [Change]
  }
  deriving (Eq, Ord, Read, Show)
$(derive makeBinary ''VersionData)

makeInitialVersion :: Monad m => [(Key, Value)] -> Transaction t m Version
makeInitialVersion initialValues = liftM Version . Transaction.newIRef . VersionData 0 Nothing $ map makeChange initialValues
  where
    makeChange (key, value) = Change key Nothing (Just value)

versionData :: Monad m => Version -> Transaction t m VersionData
versionData = Transaction.readIRef . versionIRef

newVersion :: Monad m => Version -> [Change] -> Transaction t m Version
newVersion version newChanges = do
  parentDepth <- liftM depth . versionData $ version
  liftM Version .
    Transaction.newIRef .
    VersionData (parentDepth+1) (Just version) $
    newChanges

mostRecentAncestor :: Monad m => Version -> Version -> Transaction t m Version
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
            else join $ liftM2 mostRecentAncestor (getParent aMbParentRef) (getParent bMbParentRef)
  where
    upToDepth depthToReach version = do
      VersionData curDepth curMbParentRef _curChanges <- versionData version
      if curDepth > depthToReach
        then upToDepth depthToReach =<< getParent curMbParentRef
        else return version
    getParent = maybe (fail "Non-0 depth must have a parent") return

walkUp :: Monad m => (VersionData -> Transaction t m ()) -> Version -> Version -> Transaction t m ()
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
versionsBetween :: Monad m => Version -> Version -> Transaction t m [VersionData]
versionsBetween topRef bottomRef = accumulateWalkUp [] bottomRef
  where
    accumulateWalkUp vs curRef
      | topRef == curRef  = return vs
      | otherwise         = do
        versionD <- versionData curRef
        maybe (fail "Invalid path given, hit top") (accumulateWalkUp (versionD:vs)) $
          parent versionD

-- Implement in terms of versionsBetween
walkDown :: Monad m => (VersionData -> Transaction t m ()) -> Version -> Version -> Transaction t m ()
walkDown onVersion topRef bottomRef =
  mapM_ onVersion =<< versionsBetween topRef bottomRef
