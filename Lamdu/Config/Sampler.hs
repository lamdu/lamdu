{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Config.Sampler
    ( Sampler, new
    , Sample(..), sConfig, getSample, onEachSample
    ) where

import           Control.Concurrent (threadDelay, ThreadId)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import qualified Lamdu.DataFile as DataFile
import           System.Directory (getModificationTime)

import           Lamdu.Prelude

type ModificationTime = UTCTime

-- TODO: FRP-style sampling of (mtime, file content) of the config
-- file, then map over that to Config

data Sample = Sample
    { sVersion :: ModificationTime
    , sFilePath :: FilePath
    , _sConfig :: Config
    } deriving (Eq)

Lens.makeLenses ''Sample

data Sampler = Sampler
    { _sThreadId :: ThreadId
    , sGetSample :: IO Sample
    }

onEachSample :: (Sample -> IO Sample) -> Sampler -> Sampler
onEachSample act (Sampler t get) = Sampler t (get >>= act)

getSample :: Sampler -> IO Sample
getSample = sGetSample

withMTime :: FilePath -> IO Config -> IO Sample
withMTime path act =
    do
        mtimeBefore <- getModificationTime path
        res <- act
        mtimeAfter <- getModificationTime path
        if mtimeBefore == mtimeAfter
            then Sample mtimeAfter path res & return
            else withMTime path act

load :: FilePath -> IO Sample
load configPath =
    do
        eConfig <- Aeson.eitherDecode' <$> LBS.readFile configPath
        either (fail . (msg ++)) return eConfig
    & withMTime configPath
    where
        msg = "Failed to parse config file contents at " ++ show configPath ++ ": "

maybeLoad :: Sample -> FilePath -> IO Sample
maybeLoad old configPath =
    do
        mtime <- getModificationTime configPath
        if mtime == sVersion old
            then return old
            else load configPath

new :: IO Sampler
new =
    do
        ref <-
            getConfigPath
            >>= load
            >>= E.evaluate
            >>= newMVar
        tid <-
            forkIOUnmasked . forever $
            do
                threadDelay 300000
                modifyMVar_ ref $ \old ->
                    (getConfigPath >>= maybeLoad old)
                    `E.catch` \E.SomeException {} -> return old
        return $ Sampler tid $ readMVar ref
    where
        getConfigPath = DataFile.getPath "config.json"
