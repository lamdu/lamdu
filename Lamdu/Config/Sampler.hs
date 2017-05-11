{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Config.Sampler
    ( Sampler, new
    , Sample(..), sConfigPath, sConfig, sThemePath, sTheme
    , getSample, onEachSample
    ) where

import           Control.Concurrent (threadDelay, ThreadId)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.DataFile as DataFile
import           System.Directory (getModificationTime)
import           System.FilePath (takeDirectory, (</>))

import           Lamdu.Prelude

type ModificationTime = UTCTime

-- TODO: FRP-style sampling of (mtime, file content) of the config
-- file, then map over that to Config

data Sample = Sample
    { sVersion :: [ModificationTime]
    , _sConfigPath :: FilePath
    , _sConfig :: Config
    , _sThemePath :: FilePath
    , _sTheme :: Theme
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

withMTime :: FilePath -> IO (Config, FilePath, Theme) -> IO Sample
withMTime configPath act =
    do
        (config, themePath, theme) <- act
        mtimesAfter <- traverse getModificationTime [configPath, themePath]
        Sample mtimesAfter configPath config themePath theme & return

load :: FilePath -> IO Sample
load configPath =
    do
        config <- readJson configPath
        let themePath =
                takeDirectory configPath </> "themes" </>
                Text.unpack (Config.theme config) ++ ".json"
        theme <- readJson themePath
        return (config, themePath, theme)
    & withMTime configPath
    where
        readJson path =
            Aeson.eitherDecode' <$> LBS.readFile path
            >>= either (fail . (msg ++)) return
        msg = "Failed to parse config file contents at " ++ show configPath ++ ": "

maybeReload :: Sample -> FilePath -> IO Sample
maybeReload old newConfigPath =
    do
        mtime <-
            traverse getModificationTime [old ^. sConfigPath, old ^. sThemePath]
        if mtime == sVersion old
            then return old
            else load newConfigPath

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
                    (getConfigPath >>= maybeReload old)
                    `E.catch` \E.SomeException {} -> return old
        return $ Sampler tid $ readMVar ref
    where
        getConfigPath = DataFile.getPath "config.json"
