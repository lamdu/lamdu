{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeApplications #-}
module Lamdu.Config.Sampler
    ( Sampler, new, setSelection
    , FiledConfig(..), primaryPath, fileData
    , SampleData(..), sConfig, sTheme, sLanguage
    , Sample(..), sData, sDependencyPaths
    , sConfigData, sThemeData, sLanguageData
    , getSample
    ) where

import           Control.Concurrent.Extended (ThreadId, threadDelay, forkIOUnmasked)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (WriterT, runWriterT)
import           Data.Aeson (FromJSON)
import qualified Data.Aeson.Config as AesonConfig
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import           Lamdu.Config.Folder (Selection(..))
import qualified Lamdu.Config.Folder as Folder
import           Lamdu.Config.Theme (Theme)
import           Lamdu.I18N.Language (Language)
import qualified Lamdu.Paths as Paths
import           System.Directory (getModificationTime)

import           Lamdu.Prelude

type ModificationTime = UTCTime

-- TODO: FRP-style sampling of (mtime, file content) of the config
-- file, then map over that to Config

data FiledConfig a = FiledConfig
    { _primaryPath :: FilePath
    , _fileData :: !a
    }
Lens.makeLenses ''FiledConfig

data SampleData = SampleData
    { _sConfig :: FiledConfig Config
    , _sTheme :: FiledConfig Theme
    , _sLanguage :: FiledConfig Language
    , _sDependencyPaths :: ![FilePath]
    }
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
    , setSelection :: Selection Folder.Theme -> Selection Folder.Language -> IO ()
    }

getSampleMTimes :: SampleData -> IO [ModificationTime]
getSampleMTimes sampleData =
    sampleData ^. sDependencyPaths & traverse getModificationTime

withMTime :: SampleData -> IO Sample
withMTime sampleData =
    getSampleMTimes sampleData <&> (`Sample` sampleData)

loadConfigFile :: FromJSON a => FilePath -> WriterT [FilePath] IO (FiledConfig a)
loadConfigFile path = AesonConfig.load path <&> FiledConfig path

loadPaths :: FilePath -> FilePath -> IO Sample
loadPaths themePath langPath =
    do
        config <- Paths.getDataFileName "config.json" & lift >>= loadConfigFile
        SampleData config
            <$> loadConfigFile themePath
            <*> loadConfigFile langPath
        & runWriterT
        <&> uncurry ($)
        >>= withMTime

load :: Selection Folder.Theme -> Selection Folder.Language -> IO Sample
load themeName langName =
    loadPaths
    <$> Folder.selectionToPath (Proxy @Theme) themeName
    <*> Folder.selectionToPath (Proxy @Language) langName
    & join

maybeReload :: Sample -> IO (Maybe Sample)
maybeReload (Sample oldVer old) =
    do
        mtimes <- getSampleMTimes old
        if mtimes == oldVer
            then pure Nothing
            else
            loadPaths
            (old ^. sTheme . primaryPath)
            (old ^. sLanguage . primaryPath) <&> Just

new ::
    (Sample -> IO ()) ->
    Selection Folder.Theme ->
    Selection Folder.Language -> IO Sampler
new sampleUpdated initialTheme initialLang =
    do
        ref <- load initialTheme initialLang >>= E.evaluate >>= newMVar
        tid <-
            forkIOUnmasked . forever $
            do
                threadDelay 300000
                let reloadResult old Nothing = (old, Nothing)
                    reloadResult _ (Just newSample) = (newSample, Just newSample)
                mNew <-
                    modifyMVar ref $ \old ->
                    (maybeReload old <&> reloadResult old)
                    `E.catch` \E.SomeException {} -> pure (old, Nothing)
                traverse_ sampleUpdated mNew
        pure Sampler
            { _sThreadId = tid
            , getSample = readMVar ref
            , setSelection =
                \theme lang ->
                modifyMVar ref $ \_oldVal ->
                load theme lang
                >>= E.evaluate
                <&> flip (,) ()
            }
