module Lamdu.Editor.Settings
    ( initial, newProp
    ) where

import qualified Control.Lens.Extended as Lens
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Config.Folder (Selection)
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Eval.Manager as EvalManager
import           Lamdu.I18N.Texts (Language)
import qualified Lamdu.Paths as LamduPaths
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings
import           System.FilePath ((</>))

import           Lamdu.Prelude

initial :: Selection Theme -> Selection Language -> Annotations.Mode -> Settings
initial theme lang annotationMode =
    Settings
    { _sAnnotationMode = annotationMode
    , _sSelectedTheme = theme
    , _sSelectedLanguage = lang
    , _sHelpShown = HelpNotShown
    }

settingsChangeHandler :: Sampler -> EvalManager.Evaluator -> Maybe Settings -> Settings -> IO ()
settingsChangeHandler configSampler evaluator mOld new =
    do
        lamduDir <- LamduPaths.getLamduDir
        AesonPretty.encodePretty new
            & BSL.writeFile (lamduDir </> "settings.json")
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

newProp ::
    Selection Theme -> Selection Language ->
    Annotations.Mode -> Sampler -> EvalManager.Evaluator ->
    IO (MkProperty' IO Settings)
newProp theme lang annMode configSampler evaluator =
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
        initialSettings = initial theme lang annMode
