{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Revision.Deltum.Rev.Version
    ( VersionData, depth, parent, changes
    , preventUndo
    , Version, versionIRef, versionData
    , makeInitialVersion, newVersion, mostRecentAncestor
    , walkUp, walkDown, versionsBetween, walk
    ) where

import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Rev.Change (Change(..), Key, Value)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

newtype Version m = Version { versionIRef :: IRef m (VersionData m) }
    deriving stock (Read, Show)
    deriving newtype (Eq, Ord, Binary)

data VersionData m = VersionData
    { depth :: !Int
    , parent :: !(Maybe (Version m))
    , changes :: ![Change]
    }
    deriving stock (Eq, Ord, Read, Show, Generic)
    deriving anyclass Binary

type T = Transaction

makeInitialVersion :: Monad m => [(Key, Value)] -> T m (Version m)
makeInitialVersion initialValues = fmap Version . Transaction.newIRef . VersionData 0 Nothing $ map makeChange initialValues
    where
        makeChange (key, value) = Change key Nothing (Just value)

versionData :: Monad m => Version m -> T m (VersionData m)
versionData = Transaction.readIRef . versionIRef

-- TODO: This is a hack. Used to prevent undo into initial empty
-- version. Can instead explicitly make a version when running a
-- "view" transaction
preventUndo :: Monad m => Version m -> T m ()
preventUndo version = do
    ver <- versionData version
    Transaction.writeIRef (versionIRef version)
        ver { parent = Nothing }

newVersion :: Monad m => Version m -> [Change] -> T m (Version m)
newVersion version newChanges = do
    parentDepth <- fmap depth . versionData $ version
    fmap Version .
        Transaction.newIRef .
        VersionData (parentDepth+1) (Just version) $
        newChanges

mostRecentAncestor ::
    Monad m => Version m -> Version m -> T m (Version m)
mostRecentAncestor aVersion bVersion
    | aVersion == bVersion  = pure aVersion
    | otherwise             = do
        VersionData aDepth aMbParentRef _aChanges <- versionData aVersion
        VersionData bDepth bMbParentRef _bChanges <- versionData bVersion
        case compare aDepth bDepth of
            LT -> (aVersion `mostRecentAncestor`) =<< upToDepth aDepth bVersion
            GT -> (`mostRecentAncestor` bVersion) =<< upToDepth bDepth aVersion
            EQ -> if aDepth == 0
                        then error "Two versions without common ancestor given"
                        else join $ mostRecentAncestor <$> getParent aMbParentRef <*> getParent bMbParentRef
    where
        upToDepth depthToReach version = do
            VersionData curDepth curMbParentRef _curChanges <- versionData version
            if curDepth > depthToReach
                then upToDepth depthToReach =<< getParent curMbParentRef
                else pure version
        getParent = maybe (error "Non-0 depth must have a parent") pure

walkUp ::
    (Monad m, Monoid a) =>
    (VersionData m -> T m a) ->
    Version m -> Version m -> T m a
walkUp onVersion topRef bottomRef
    | bottomRef == topRef  = pure mempty
    | otherwise            = do
        versionD <- versionData bottomRef
        result <- onVersion versionD
        parent versionD
            & maybe
                (error "Invalid path given, hit top")
                (walkUp onVersion topRef)
            <&> mappend result

-- We can't directly walkDown (we don't have references pointing
-- downwards... But we can generate a list of versions by walking up
-- and accumulating a reverse list)
versionsBetween ::
    Monad m => Version m -> Version m ->
    T m [VersionData m]
versionsBetween topRef = accumulateWalkUp []
    where
        accumulateWalkUp vs curRef
            | topRef == curRef  = pure vs
            | otherwise         = do
                versionD <- versionData curRef
                maybe (error "Invalid path given, hit top") (accumulateWalkUp (versionD:vs)) $
                    parent versionD

walk ::
    (Monad m, Monoid a) =>
    (VersionData m -> T m a) ->
    (VersionData m -> T m a) ->
    Version m -> Version m -> T m a
walk applyBackward applyForward srcVersion destVersion =
        do
                mraIRef <- mostRecentAncestor srcVersion destVersion
                mappend
                        <$> walkUp applyBackward mraIRef srcVersion
                        <*> walkDown applyForward mraIRef destVersion

-- Implement in terms of versionsBetween
walkDown ::
    (Monad m, Monoid a) =>
    (VersionData m -> T m a) ->
    Version m -> Version m -> T m a
walkDown onVersion topRef bottomRef =
    fmap mconcat . traverse onVersion =<< versionsBetween topRef bottomRef
