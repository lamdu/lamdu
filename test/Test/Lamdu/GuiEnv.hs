{-# LANGUAGE TemplateHaskell #-}

module Test.Lamdu.GuiEnv (Env(..), make, dummyAnchors) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit(..))
import qualified Data.Aeson.Config as AesonConfig
import           Data.Functor.Identity (Identity(..))
import           Data.Property (MkProperty(..), Property(..))
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Draw (Color(..))
import           GUI.Momentu.Element (HasAnimIdPrefix(..))
import           GUI.Momentu.Font (openFont)
import           GUI.Momentu.State (HasState(..), HasCursor, GUIState(..))
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing(..))
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config, HasConfig(..))
import           Lamdu.Config.Theme (Theme, HasTheme(..), baseTextSize, fonts)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Font (fontDefault)
import qualified Lamdu.Paths as Paths
import           Lamdu.Settings (HasSettings(..), Settings, initial)
import           Lamdu.Style (HasStyle(..), Style)
import qualified Lamdu.Style as Style
import qualified Test.Lamdu.Theme as TestTheme

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
    }
Lens.makeLenses ''Env
instance HasTheme Env where theme = eTheme
instance HasStdSpacing Env where stdSpacing = eSpacing
instance TextView.HasStyle Env where style = TextEdit.style . TextView.style
instance HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance HasCursor Env
instance HasState Env where state = eState
instance HasConfig Env where config = eConfig
instance HasSettings Env where settings = eSettings
instance TextEdit.HasStyle Env where style = eTextEditStyle
instance HasStyle Env where style = eStyle

make :: IO Env
make =
    do
        testConfig <- Paths.getDataFileName "config.json" >>= AesonConfig.load
        testTheme <- TestTheme.load
        font <-
            testTheme ^. fonts . fontDefault & Paths.getDataFileName
            >>= openFont (testTheme ^. baseTextSize)
        pure Env
            { _eTheme = testTheme
            , _eConfig = testConfig
            , _eState =
                GUIState
                { _sCursor = "dummy"
                , _sWidgetStates = mempty
                }
            , _eSettings = initial
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
