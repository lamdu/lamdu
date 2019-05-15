{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeApplications #-}

module Test.Lamdu.GuiEnv (Env(..), make, makeLang, dummyAnchors) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Unit (Unit(..))
import qualified Data.Aeson.Config as AesonConfig
import           Data.Functor.Identity (Identity(..))
import           Data.Has (Has(..))
import           Data.Property (MkProperty(..), Property(..))
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Draw (Color(..))
import           GUI.Momentu.Element (HasAnimIdPrefix(..))
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Font (openFont, LCDSubPixelEnabled(..))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.State (HasCursor, GUIState(..))
import qualified GUI.Momentu.Widgets.Choice as Choice
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing(..))
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Config (Config, HasConfig(..))
import           Lamdu.Config.Folder (Selection(..))
import           Lamdu.Config.Theme (Theme, HasTheme(..), baseTextSize, fonts)
import qualified Lamdu.Config.Theme.Fonts as Fonts
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Language (Language, HasLanguage(..))
import           Lamdu.Name (HasNameTexts(..))
import qualified Lamdu.Paths as Paths
import           Lamdu.Settings (HasSettings(..), Settings(..))
import           Lamdu.Style (HasStyle(..), Style)
import qualified Lamdu.Style as Style
import qualified Test.Lamdu.Config as TestConfig

import           Test.Lamdu.Prelude

data Env =
    Env
    { _eTheme :: Theme
    , _eSpacing :: Vector2 Double
    , _eAnimIdPrefix :: Anim.AnimId
    , _eState :: GUIState
    , _eConfig :: Config
    , _eSettings :: Settings
    , _eStyle :: Style
    , _eTextEditStyle :: TextEdit.Style
    , _eDirLayout :: Dir.Layout
    , _eLanguage :: Language
    }
Lens.makeLenses ''Env
instance HasTheme Env where theme = eTheme
instance HasStdSpacing Env where stdSpacing = eSpacing
instance Has TextView.Style Env where has = has @TextEdit.Style . has
instance HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance HasCursor Env
instance Has GUIState Env where has = eState
instance HasConfig Env where config = eConfig
instance HasSettings Env where settings = eSettings
instance Has TextEdit.Style Env where has = eTextEditStyle
instance HasStyle Env where style = eStyle
instance Has Dir.Layout Env where has = eDirLayout
instance Dir.HasTexts Env where texts = language . Dir.texts
instance Glue.HasTexts Env where texts = language . Glue.texts
instance EventMap.HasTexts Env where texts = language . EventMap.texts
instance Choice.HasTexts Env where texts = language . Choice.texts
instance TextEdit.HasTexts Env where texts = language . TextEdit.texts
instance Grid.HasTexts Env where texts = language . Grid.texts
instance Menu.HasTexts Env where texts = language . Menu.texts
instance SearchMenu.HasTexts Env where texts = language . SearchMenu.texts
instance HasNameTexts Env where nameTexts = language . nameTexts
instance Has LangId Env where has = language . has
instance HasLanguage Env where language = eLanguage

makeLang :: IO Language
makeLang = TestConfig.loadConfigObject "english"

make :: IO Env
make =
    do
        testConfig <-
            Paths.getDataFileName "config.json"
            >>= Writer.evalWriterT . AesonConfig.load
        testTheme <- TestConfig.loadConfigObject "dark"
        testLang <- makeLang
        font <-
            testTheme ^. fonts . Fonts.base & Paths.getDataFileName
            >>= openFont LCDSubPixelDisabled (testTheme ^. baseTextSize)
        pure Env
            { _eTheme = testTheme
            , _eConfig = testConfig
            , _eState =
                GUIState
                { _sCursor = WidgetIds.defaultCursor
                , _sWidgetStates = mempty
                }
            , _eSettings =
                Settings
                { _sAnnotationMode = Annotations.Evaluation
                , _sSelectedTheme = Selection "dark"
                , _sSelectedLanguage = Selection "english"
                , _sHelpShown = HelpNotShown
                }
            , _eStyle = Style.make (font <$ testTheme ^. fonts) testTheme
            , _eSpacing = 1
            , _eTextEditStyle =
                TextEdit.Style
                { TextEdit._sTextViewStyle =
                    TextView.Style
                    { TextView._styleColor = Color 1 1 1 1
                    , TextView._styleFont = font
                    , TextView._styleUnderline = Nothing
                    }
                , TextEdit._sCursorColor = Color 1 1 1 1
                , TextEdit._sCursorWidth = 1
                , TextEdit._sEmptyStringsColors = pure (Color 1 1 1 1)
                }
            , _eAnimIdPrefix = []
            , _eDirLayout = Dir.LeftToRight -- TODO: Test other directions
            , _eLanguage = testLang
            }

prop :: a -> MkProperty Identity Unit a
prop x = Property x (const Unit) & Identity & MkProperty

dummyAnchors :: Anchors.GuiAnchors Identity Unit
dummyAnchors =
    Anchors.Gui
    { Anchors.preJumps = prop []
    , Anchors.preGuiState = prop dummyState
    , Anchors.postGuiState = prop dummyState
    }
    where
        dummyState =
            GUIState
            { _sCursor = "dummy"
            , _sWidgetStates = mempty
            }
