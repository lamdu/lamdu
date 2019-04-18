{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Lamdu.Config.Sampler
    ( Sampler, new
    , Sample(..), sConfigPath, sConfig
    , sThemePath, sTheme, sTextsPath, sTexts, setSelection
    , getSample
    ) where

import           Control.Concurrent.Extended (ThreadId, threadDelay, forkIOUnmasked)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Data.Aeson (FromJSON)
import qualified Data.Aeson.Config as AesonConfig
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import           Lamdu.Config.Folder (HasConfigFolder(..), Selection(..))
import           Lamdu.Config.Theme (Theme)
import           Lamdu.I18N.Texts (Texts)
import qualified Lamdu.Paths as Paths
import           System.Directory (getModificationTime)
import           System.FilePath (takeDirectory, takeFileName, dropExtension, (</>))

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
    , _sTextsPath :: FilePath
    , _sTexts :: Texts
    } deriving (Eq)
Lens.makeLenses ''Sample

data Sampler = Sampler
    { _sThreadId :: ThreadId
    , getSample :: IO Sample
    , setSelection :: Selection Theme -> Selection Texts -> IO ()
    }

withMTime :: FilePath -> IO (Config, (FilePath, Theme), (FilePath, Texts)) -> IO Sample
withMTime _sConfigPath act =
    do
        (_sConfig, (_sThemePath, _sTheme), (_sTextsPath, _sTexts)) <- act
        sVersion <- traverse getModificationTime [_sConfigPath, _sThemePath]
        pure Sample{..}

loadFromFolder :: (HasConfigFolder a, FromJSON a) => FilePath -> Selection a -> IO (FilePath, a)
loadFromFolder configPath selection =
    AesonConfig.load path <&> (,) path
    where
        path =
            takeDirectory configPath </> configFolder selection </>
            Text.unpack (getSelection selection) ++ ".json"

load :: Selection Theme -> Selection Texts -> FilePath -> IO Sample
load themeName langName configPath =
    do
        config <- AesonConfig.load configPath
        (,,) config
            <$> loadFromFolder configPath themeName
            <*> loadFromFolder configPath langName
    & withMTime configPath

maybeReload :: Sample -> FilePath -> IO (Maybe Sample)
maybeReload old newConfigPath =
    do
        mtime <-
            traverse getModificationTime [old ^. sConfigPath, old ^. sThemePath]
        if mtime == sVersion old
            then pure Nothing
            else load (f sThemePath) (f sTextsPath) newConfigPath <&> Just
    where
        f l = old ^. l & takeFileName & dropExtension & Text.pack & Selection

new :: (Sample -> IO ()) -> Selection Theme -> Selection Texts -> IO Sampler
new sampleUpdated initialTheme initialLang =
    do
        ref <-
            getConfigPath
            >>= load initialTheme initialLang
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
            , setSelection =
                \theme lang ->
                takeMVar ref
                >> getConfigPath
                >>= load theme lang
                >>= E.evaluate
                >>= putMVar ref
            }
    where
        getConfigPath = Paths.getDataFileName "config.json"
