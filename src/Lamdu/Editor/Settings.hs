module Lamdu.Editor.Settings
    ( initial, newProp
    ) where

import qualified Control.Lens.Extended as Lens
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
import           Lamdu.I18N.Texts (Texts)
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings

import           Lamdu.Prelude

initial :: Selection Theme -> Selection Texts -> Annotations.Mode -> Settings
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
        whenChanged Settings.sAnnotationMode $
            case new ^. Settings.sAnnotationMode of
            Annotations.Evaluation -> EvalManager.start evaluator
            _ -> EvalManager.stop evaluator
        whenChanged Settings.sSelectedTheme setSelection
        whenChanged Settings.sSelectedLanguage setSelection
    where
        setSelection =
            ConfigSampler.setSelection configSampler
            (new ^. Settings.sSelectedTheme) (new ^. Settings.sSelectedLanguage)
        whenChanged lens f =
            case mOld of
            Nothing -> f
            Just old -> when (old ^. lens /= new ^. lens) f

newProp ::
    Selection Theme -> Selection Texts ->
    Annotations.Mode -> Sampler -> EvalManager.Evaluator -> IO (MkProperty' IO Settings)
newProp theme lang annMode configSampler evaluator =
    do
        settingsChangeHandler configSampler evaluator Nothing initialSettings
        newIORef initialSettings <&> Property.fromIORef
            <&> Property.mkProperty . Lens.mapped .
                Lens.filteredBy Property.pVal <.> Property.pSet . Lens.imapped %@~
                \(oldVal, newVal) ->
                    -- Callback to notify update  AFTER we set the property:
                    (*> settingsChangeHandler configSampler evaluator (Just oldVal) newVal)
    where
        initialSettings = initial theme lang annMode
