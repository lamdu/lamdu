module Lamdu.Editor.Settings
    ( newProp, read
    ) where

import           Control.Exception.Lens (handling, _IOException)
import qualified Control.Lens.Extended as Lens
import           Control.Monad.Except (runExceptT, throwError)
import           Data.Aeson (eitherDecode')
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Eval.Manager as EvalManager
import qualified Lamdu.Paths as LamduPaths
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)

import           Lamdu.Prelude hiding (read)

getSettingsPath :: IO FilePath
getSettingsPath = LamduPaths.getLamduDir <&> (</> "settings.json")

settingsChangeHandler :: Sampler -> EvalManager.Evaluator -> Maybe Settings -> Settings -> IO ()
settingsChangeHandler configSampler evaluator mOld new =
    do
        settingsPath <- getSettingsPath
        AesonPretty.encodePretty new & BSL.writeFile settingsPath
        when (didChange Settings.sAnnotationMode) $
            case new ^. Settings.sAnnotationMode of
            Annotations.Evaluation -> EvalManager.start evaluator
            _ -> EvalManager.stop evaluator
        when
            ( didChange Settings.sSelectedTheme
                || didChange Settings.sSelectedLanguage
            ) setSelection
    where
        setSelection =
            ConfigSampler.setSelection configSampler
            (new ^. Settings.sSelectedTheme) (new ^. Settings.sSelectedLanguage)
        didChange lens =
            case mOld of
            Nothing -> True
            Just old -> old ^. lens /= new ^. lens

readFrom :: FilePath -> IO (Either String Settings)
readFrom path =
    do
        fileData <-
            BSL.readFile path & lift
            & handling _IOException (throwError . ioErr)
        eitherDecode' fileData & either (throwError . msg) pure
        & runExceptT
    where
        ioErr err = show path <> ": " <> show err
        msg err = show path <> " has bad JSON: " <> err

read :: IO Settings
read =
    getSettingsPath >>= readFrom
    >>= \case
    Right settings -> pure settings
    Left err ->
        do
            hPutStrLn stderr $ err <> ": Falling back to default settings..."
            LamduPaths.getDataFileName "default-settings.json" >>= readFrom
                >>= either fail pure

newProp ::
    Settings -> Sampler -> EvalManager.Evaluator -> IO (MkProperty' IO Settings)
newProp initialSettings configSampler evaluator =
    do
        handleChange Nothing initialSettings
        newIORef initialSettings <&> Property.fromIORef
            <&> Property.mkProperty . Lens.mapped .
                Lens.filteredBy Property.pVal <.> Property.pSet . Lens.imapped %@~
                \(oldVal, newVal) ->
                    -- Callback to notify update  AFTER we set the property:
                    (*> handleChange (Just oldVal) newVal)
    where
        handleChange = settingsChangeHandler configSampler evaluator
