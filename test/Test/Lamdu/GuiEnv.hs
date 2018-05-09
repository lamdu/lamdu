{-# LANGUAGE TemplateHaskell #-}

module Test.Lamdu.GuiEnv (Env(..), make) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Draw (Color(..))
import           GUI.Momentu.Element (HasAnimIdPrefix(..))
import           GUI.Momentu.Font (openFont)
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing(..))
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme, HasTheme(..), baseTextSize, fonts)
import           Lamdu.Font (fontDefault)
import qualified Lamdu.Paths as Paths
import qualified Test.Lamdu.Theme as TestTheme

import           Test.Lamdu.Prelude

data Env =
    Env
    { _eTheme :: Theme
    , _eSpacing :: Vector2 Double
    , _eTextViewStyle :: TextView.Style
    , _eAnimIdPrefix :: Anim.AnimId
    }
Lens.makeLenses ''Env
instance HasTheme Env where theme = eTheme
instance HasStdSpacing Env where stdSpacing = eSpacing
instance TextView.HasStyle Env where style = eTextViewStyle
instance HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix

make :: IO Env
make =
    do
        testTheme <- TestTheme.load
        font <-
            testTheme ^. fonts . fontDefault & Paths.getDataFileName
            >>= openFont (testTheme ^. baseTextSize)
        pure Env
            { _eTheme = testTheme
            , _eSpacing = 1
            , _eTextViewStyle =
                TextView.Style
                { TextView._styleColor = Color 1 1 1 1
                , TextView._styleFont = font
                , TextView._styleUnderline = Nothing
                }
            , _eAnimIdPrefix = []
            }
