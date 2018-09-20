{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Config.Sampler
    ( Sampler, new
    , Sample(..), sConfigPath, sConfig
    , defaultFontPath
    , sThemePath, sTheme, setTheme
    , getSample
    ) where

import           Control.Concurrent.Extended (ThreadId, threadDelay, forkIOUnmasked)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.Aeson.Config as AesonConfig
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Paths as Paths
import           System.Directory (getModificationTime)
import           System.FilePath (takeDirectory, takeFileName, dropExtension, (</>))
import qualified System.FilePath as FilePath

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
    , getSample :: IO Sample
    , setTheme :: Text -> IO ()
    }

withMTime :: FilePath -> IO (Config, FilePath, Theme) -> IO Sample
withMTime configPath act =
    do
        (config, themePath, theme) <- act
        mtimesAfter <- traverse getModificationTime [configPath, themePath]
        Sample mtimesAfter configPath config themePath theme & pure

calcThemePath :: FilePath -> Text -> FilePath
calcThemePath configPath theme =
    takeDirectory configPath </> "themes" </>
    Text.unpack theme ++ ".json"

load :: Text -> FilePath -> IO Sample
load themeName configPath =
    do
        config <- AesonConfig.load configPath
        theme <- AesonConfig.load themePath
        pure (config, themePath, theme)
    & withMTime configPath
    where
        themePath = calcThemePath configPath themeName

maybeReload :: Sample -> FilePath -> IO (Maybe Sample)
maybeReload old newConfigPath =
    do
        mtime <-
            traverse getModificationTime [old ^. sConfigPath, old ^. sThemePath]
        if mtime == sVersion old
            then pure Nothing
            else load theme newConfigPath <&> Just
    where
        theme = old ^. sThemePath & takeFileName & dropExtension & Text.pack

new :: (Sample -> IO ()) -> Text -> IO Sampler
new sampleUpdated initialTheme =
    do
        ref <-
            getConfigPath
            >>= load initialTheme
            >>= E.evaluate
            >>= newMVar
        tid <-
            forkIOUnmasked . forever $
            do
                threadDelay 300000
                let reloadResult old Nothing = (old, Nothing)
                    reloadResult _ (Just newSample) = (newSample, Just newSample)
                mNew <-
                    modifyMVar ref $ \old ->
                    (getConfigPath >>= maybeReload old <&> reloadResult old)
                    `E.catch` \E.SomeException {} -> pure (old, Nothing)
                traverse_ sampleUpdated mNew
        pure Sampler
            { _sThreadId = tid
            , getSample = readMVar ref
            , setTheme =
                \theme ->
                takeMVar ref
                >> getConfigPath
                >>= load theme
                >>= E.evaluate
                >>= putMVar ref
            }
    where
        getConfigPath = Paths.getDataFileName "config.json"

defaultFontPath :: Sample -> FilePath
defaultFontPath sample =
    configDir </> "fonts/Purisa.ttf"
    where
        configDir = FilePath.takeDirectory (sample ^. sConfigPath)
