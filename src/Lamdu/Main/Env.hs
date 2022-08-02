-- | The Environment threaded in Lamdu main
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, TypeFamilies #-}
module Lamdu.Main.Env
    ( Env(..)
    , evalRes
    , exportActions
    , config
    , theme
    , settings
    , sugars
    , sprites
    , style
    , mainLoop
    , animIdPrefix
    , debugMonitors
    , cachedFunctions
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu (AnimId, ModKey, GUIState)
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Draw (Sprite)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Sprites (Sprites(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Debug as Debug
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Language (Language)
import           Lamdu.I18N.Texts (Texts)
import           Lamdu.Settings (Settings(..), sAnnotationMode)
import           Lamdu.Style (Style)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.Config as SugarConfig

import           Lamdu.Prelude

data Env = Env
    { _evalRes :: GUIMain.EvalResults
    , _exportActions :: GUIMain.ExportActions ViewM
    , _config :: Config ModKey
    , _theme :: Theme
    , _sprites :: Sprites Sprite
    , _settings :: Property IO Settings
    , _sugars :: Property IO (SugarConfig.Sugars Bool)
    , _style :: Style.Style
    , _mainLoop :: MainLoop.Env
    , _animIdPrefix :: AnimId
    , _debugMonitors :: Debug.Monitors
    , _cachedFunctions :: Cache.Functions
    , _language :: Language
    , _codeAnchors :: Anchors.CodeAnchors ViewM
    }
Lens.makeLenses ''Env

instance Anchors.HasCodeAnchors Env ViewM where codeAnchors = codeAnchors
instance Element.HasAnimIdPrefix Env where animIdPrefix = animIdPrefix
instance GuiState.HasCursor Env
instance Has (GUIMain.ExportActions ViewM) Env where has = exportActions
instance Has (Sprites Sprite) Env where has = sprites
instance Has (t Text) (Texts Text) => Has (t Text) Env where has = language . has
instance Has Annotations.Mode Env where has = has . sAnnotationMode
instance Has Cache.Functions Env where has = cachedFunctions
instance Has Debug.Monitors Env where has = debugMonitors
instance Has Dir.Layout Env where has = language . has
instance Has GUIMain.EvalResults Env where has = evalRes
instance Has GUIState Env where has = mainLoop . has
instance Has Hover.Style Env where has = theme . has
instance Has LangId Env where has = language . has
instance Has Language Env where has = language
instance Has MainLoop.Env Env where has = mainLoop
instance Has Menu.Style Env where has = theme . Theme.menu
instance Has SearchMenu.TermStyle Env where has = theme . Theme.searchTerm
instance Has Settings Env where has = settings . Property.pVal
instance Has Style Env where has = style
instance Has (SugarConfig.Sugars Bool) Env where has = sugars . Property.pVal
instance Has TextEdit.Style Env where has = style . Style.base
instance Has TextView.Style Env where has = has @TextEdit.Style . has
instance Has Theme Env where has = theme
instance Has VCConfig.Theme Env where has = has . Theme.versionControl
instance Spacer.HasStdSpacing Env where stdSpacing = has . Theme.stdSpacing
instance key ~ ModKey => Has (Config key) Env where has = config
instance key ~ ModKey => Has (Menu.Config key) Env where has = has . SearchMenu.configMenu
instance key ~ ModKey => Has (SearchMenu.Config key) Env where has = Config.hasConfig . Config.searchMenu
instance key ~ ModKey => Has (StdKeys.DirKeys key) Env where has = config . Config.dirKeys
instance key ~ ModKey => Has (TextEdit.Keys key) Env where has = config . Config.textEdit
instance key ~ ModKey => Has (VCConfig.Config key) Env where has = Config.hasConfig . Config.versionControl
