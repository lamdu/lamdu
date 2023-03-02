-- | Widget to edit the settings
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Lamdu.GUI.Settings
    ( StatusWidgets(..), annotationWidget, themeWidget, languageWidget, helpWidget
    , TitledSelection(..), title, selection
    , makeStatusWidgets
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property, composeLens)
import           GUI.Momentu (WithTextPos(..))
import           GUI.Momentu.Element.Id (asElemId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.I18N as Texts
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Ann
import qualified Lamdu.Config as Config
import           Lamdu.Config.Folder (Selection)
import qualified Lamdu.Config.Folder as Folder
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.Sprites as Sprites
import           Lamdu.GUI.StatusBar.Common (StatusWidget)
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.StatusBar as Texts
import           Lamdu.Settings (Settings)
import qualified Lamdu.Settings as Settings

import           Lamdu.Prelude

data StatusWidgets a = StatusWidgets
    { _annotationWidget :: a
    , _themeWidget      :: a
    , _languageWidget   :: a
    , _helpWidget       :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''StatusWidgets

data TitledSelection a = TitledSelection
    { _title :: !Text
    , _selection :: !(Selection a)
    }
Lens.makeLenses ''TitledSelection

makeAnnotationsSwitcher :: _ => Property f Ann.Mode -> m (StatusBar.StatusWidget f)
makeAnnotationsSwitcher annotationModeProp =
    do
        mk0 <- Styled.mkFocusableLabel
        mk1 <- Styled.mkFocusableLabel
        [ (Ann.Evaluation, mk0 Texts.evaluation)
            , (Ann.Types, mk1 Texts.sbTypes)
            , (Ann.None, mk1 Texts.sbNone)
            ]
            & StatusBar.makeSwitchStatusWidget
                (Styled.sprite Sprites.pencilLine <&> WithTextPos 0)
                Texts.sbAnnotations Texts.sbSwitchAnnotations
            Config.nextAnnotationModeKeys annotationModeProp

makeStatusWidgets ::
    (MonadReader env m, _) =>
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    Property f Settings -> m (StatusWidgets (StatusWidget f))
makeStatusWidgets themeNames langNames prop =
    sequenceA
    StatusWidgets
    { _annotationWidget =
        makeAnnotationsSwitcher (composeLens Settings.sAnnotationMode prop)
        & local (Element.elemIdPrefix <>~ "Annotations Mode")
    , _themeWidget =
        traverse opt themeNames
        >>= StatusBar.makeSwitchStatusWidget
        (Styled.sprite Sprites.theme <&> WithTextPos 0)
        Texts.sbTheme Texts.sbSwitchTheme
        Config.changeThemeKeys themeProp
        & local (Element.elemIdPrefix <>~ "Theme Select")
    , _languageWidget =
       traverse opt langNames
        >>= StatusBar.makeSwitchStatusWidget
        (Styled.sprite Sprites.earthGlobe <&> WithTextPos 0)
        Texts.language Texts.sbSwitchLanguage
        Config.changeLanguageKeys langProp
        & local (Element.elemIdPrefix <>~ "Language Select")
    , _helpWidget =
        helpVals
        >>= StatusBar.makeSwitchStatusWidget
        (pure Element.empty)
        Texts.sbHelp Texts.sbSwitchHelp
        Config.helpKeys helpProp
        & local (Element.elemIdPrefix <>~ "Help Select")
    }
    where
        helpHiddenSprite = Styled.sprite Sprites.help
        helpShownSprite =
            do
                iconTint <- Lens.view (has . Theme.help . Theme.helpShownIconTint)
                Styled.sprite Sprites.help <&> Element.tint iconTint
        makeFocusable elemId mkView =
            mkView >>=  Widget.makeFocusableView elemId
            <&> WithTextPos 0
            & local (Element.elemIdPrefix .~ elemId)
        opt sel =
            Lens.view Element.elemIdPrefix <&> (<> asElemId (sel ^. selection))
            >>= TextView.makeFocusable (sel ^. title)
            <&> (,) (sel ^. selection)
        helpVals =
            Lens.sequenceOf (Lens.traverse . _2)
            [ (HelpNotShown, makeFocusable "Help hidden" helpHiddenSprite)
            , (HelpShown, makeFocusable "Help shown" helpShownSprite)
            ]
        themeProp = composeLens Settings.sSelectedTheme prop
        langProp = composeLens Settings.sSelectedLanguage prop
        helpProp = composeLens Settings.sHelpShown prop
