{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeApplications #-}
module Lamdu.Config.Sampler
    ( Sampler, new, setSelection
    , FiledConfig(..), primaryPath, fileData
    , SampleData(..), sConfig, sTheme, sLanguage, sSprites, sDependencyPaths
    , Sample(..), sData
    , sConfigData, sThemeData, sLanguageData, sSpritesData
    , getSample
    ) where

import           Control.Concurrent.Extended (ThreadId, threadDelay, forkIOUnmasked)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (WriterT, runWriterT, tell)
import           Data.Aeson (FromJSON)
import qualified Data.Aeson.Config as AesonConfig
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified Graphics.DrawingCombinators as Draw
import           Lamdu.Config (Config)
import           Lamdu.Config.Folder (Selection(..))
import qualified Lamdu.Config.Folder as Folder
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Sprites (Sprites)
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
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''FiledConfig

data SampleData = SampleData
    { _sConfig :: FiledConfig (Config MetaKey)
    , _sTheme :: FiledConfig Theme
    , _sLanguage :: FiledConfig Language
    , _sSprites :: Sprites Draw.Sprite
    , _sDependencyPaths :: ![FilePath]
    }
Lens.makeLenses ''SampleData

data Sample = Sample
    { sVersion :: [ModificationTime]
    , _sData :: !SampleData
    }
Lens.makeLenses ''Sample

sConfigData :: Lens' Sample (Config MetaKey)
sConfigData = sData . sConfig . fileData

sThemeData :: Lens' Sample Theme
sThemeData = sData . sTheme . fileData

sLanguageData :: Lens' Sample Language
sLanguageData = sData . sLanguage . fileData

sSpritesData :: Lens' Sample (Sprites Draw.Sprite)
sSpritesData = sData . sSprites

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

loadSprite :: FilePath -> WriterT [FilePath] IO Draw.Sprite
loadSprite relPath =
    do
        path <- Folder.spritePath relPath & lift
        tell [path] *> lift (Draw.openSprite path)

toMetaKey :: String -> MetaKey
toMetaKey s =
    MetaKey.parse (Text.pack s) & fromMaybe (error ("Bad key string: " ++ show s))

loadPaths :: FilePath -> FilePath -> IO Sample
loadPaths themePath langPath =
    do
        config <-
            Paths.getDataFileName "config.json" & lift >>= loadConfigFile
            <&> Lens.mapped . Lens.mapped %~ toMetaKey
        theme <- loadConfigFile themePath
        SampleData config theme
            <$> loadConfigFile langPath
            <*> traverse loadSprite (theme ^. fileData . Theme.sprites)
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
