{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Lamdu.Config.Sampler
    ( Sampler, new, setSelection
    , FiledConfig(..), filePath, fileData
    , SampleData(..), sConfig, sTheme, sLanguage
    , Sample(..), sData
    , sConfigData, sThemeData, sLanguageData
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
import           Lamdu.I18N.Texts (Language)
import qualified Lamdu.Paths as Paths
import           System.Directory (getModificationTime)
import           System.FilePath (takeDirectory, takeFileName, dropExtension, (</>))

import           Lamdu.Prelude

type ModificationTime = UTCTime

-- TODO: FRP-style sampling of (mtime, file content) of the config
-- file, then map over that to Config

data FiledConfig a = FiledConfig
    { _filePath :: !FilePath
    , _fileData :: !a
    } deriving (Eq)
Lens.makeLenses ''FiledConfig

data SampleData = SampleData
    { _sConfig :: FiledConfig Config
    , _sTheme :: FiledConfig Theme
    , _sLanguage :: FiledConfig Language
    } deriving (Eq)
Lens.makeLenses ''SampleData

data Sample = Sample
    { sVersion :: [ModificationTime]
    , _sData :: !SampleData
    }
Lens.makeLenses ''Sample

sConfigData :: Lens' Sample Config
sConfigData = sData . sConfig . fileData

sThemeData :: Lens' Sample Theme
sThemeData = sData . sTheme . fileData

sLanguageData :: Lens' Sample Language
sLanguageData = sData . sLanguage . fileData

data Sampler = Sampler
    { _sThreadId :: ThreadId
    , getSample :: IO Sample
    , setSelection :: Selection Theme -> Selection Language -> IO ()
    }

sampleFilePaths :: Lens.Traversal' SampleData FilePath
sampleFilePaths f (SampleData conf theme language) =
    SampleData
    <$> filePath f conf
    <*> filePath f theme
    <*> filePath f language

getSampleMTimes :: SampleData -> IO [ModificationTime]
getSampleMTimes sampleData =
    sampleData ^.. sampleFilePaths & traverse getModificationTime

withMTime :: FilePath -> IO (Config, FiledConfig Theme, FiledConfig Language) -> IO Sample
withMTime _sConfigPath act =
    do
        (_sConfigData, _sTheme, _sLanguage) <- act
        let _sConfig = FiledConfig _sConfigPath _sConfigData
        let sampleData = SampleData{..}
        mtimes <- getSampleMTimes sampleData
        Sample mtimes sampleData & pure

loadFromFolder ::
    (HasConfigFolder a, FromJSON a) =>
    FilePath -> Selection a -> IO (FiledConfig a)
loadFromFolder configPath selection =
    AesonConfig.load path <&> FiledConfig path
    where
        path =
            takeDirectory configPath </> configFolder selection </>
            Text.unpack (getSelection selection) ++ ".json"

load :: Selection Theme -> Selection Language -> FilePath -> IO Sample
load themeName langName configPath =
    do
        config <- AesonConfig.load configPath
        (,,) config
            <$> loadFromFolder configPath themeName
            <*> loadFromFolder configPath langName
    & withMTime configPath

maybeReload :: Sample -> FilePath -> IO (Maybe Sample)
maybeReload (Sample oldVer old) newConfigPath =
    do
        mtimes <- getSampleMTimes old
        if mtimes == oldVer
            then pure Nothing
            else load (f sTheme) (f sLanguage) newConfigPath <&> Just
    where
        f l = old ^. l . filePath & takeFileName & dropExtension & Text.pack & Selection

new :: (Sample -> IO ()) -> Selection Theme -> Selection Language -> IO Sampler
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
