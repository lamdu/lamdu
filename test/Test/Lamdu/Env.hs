{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Lamdu.Env
    ( Env(..), make, makeLang, dummyAnchors
    , EvalResults
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Unit (Unit(..))
import qualified Data.Aeson.Config as AesonConfig
import           Data.CurAndPrev (CurAndPrev)
import           Data.Functor.Identity (Identity(..))
import           Data.Property (MkProperty(..), Property(..))
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Draw (Color(..))
import           GUI.Momentu.Element (HasAnimIdPrefix(..))
import           GUI.Momentu.Font (openFont, LCDSubPixelEnabled(..))
import           GUI.Momentu.State (HasCursor, GUIState(..))
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing(..))
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Folder (Selection(..))
import           Lamdu.Config.Theme (Theme, baseTextSize, fonts)
import qualified Lamdu.Config.Theme.Fonts as Fonts
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Language (Language)
import           Lamdu.I18N.Texts (Texts)
import qualified Lamdu.Paths as Paths
import           Lamdu.Settings (Settings(..), sAnnotationMode)
import           Lamdu.Style (Style)
import qualified Lamdu.Style.Make as MakeStyle
import qualified Lamdu.Sugar.Config as SugarConfig
import qualified Test.Lamdu.Config as TestConfig

import           Test.Lamdu.Prelude

type EvalResults = CurAndPrev (EvalResults.EvalResults (ValI ViewM))

data Env =
    Env
    { _eTheme :: Theme
    , _eSpacing :: Vector2 Double
    , _eAnimIdPrefix :: Anim.AnimId
    , _eState :: GUIState
    , _eConfig :: Config
    , _eSettings :: Settings
    , _eTasksMonitor :: Debug.Monitors
    , _eResults :: EvalResults
    , _eCacheFunctions :: Cache.Functions
    , _eStyle :: Style
    , _eTextEditStyle :: TextEdit.Style
    , _eDirLayout :: Dir.Layout
    , _eLanguage :: Language
    }
Lens.makeLenses ''Env
instance Has Theme Env where has = eTheme
instance HasStdSpacing Env where stdSpacing = eSpacing
instance Has TextView.Style Env where has = has @TextEdit.Style . has
instance HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance HasCursor Env
instance Has GUIState Env where has = eState
instance Has Config Env where has = eConfig
instance Has Settings Env where has = eSettings
instance Has TextEdit.Style Env where has = eTextEditStyle
instance Has Style Env where has = eStyle
instance Has Dir.Layout Env where has = eDirLayout
instance Has LangId Env where has = eLanguage . has
instance Has Language Env where has = eLanguage
instance Has Debug.Monitors Env where has = eTasksMonitor
instance Has Cache.Functions Env where has = eCacheFunctions
instance Has SugarConfig.Config Env where has = has . Config.sugar
instance Has EvalResults Env where has = eResults
instance Has Annotations.Mode Env where has = has . sAnnotationMode
instance Has (t Text) (Texts Text) => Has (t Text) Env where has = eLanguage . has

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
        cache <- Cache.make <&> snd
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
            , _eTasksMonitor = Debug.noopMonitors
            , _eResults = pure EvalResults.empty
            , _eCacheFunctions = cache
            , _eStyle = MakeStyle.make (font <$ testTheme ^. fonts) testTheme
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
