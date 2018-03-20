{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
-- | The themes/ config format
module Lamdu.Config.Theme
    ( module Exported
    , Help(..), Hole(..), Eval(..), ToolTip(..), StatusBar(..)
    , Theme(..), stdSpacing, menu, versionControl
    , HasTheme(..)
    ) where

import qualified Control.Lens as Lens
#ifndef NO_CODE
import           Data.Aeson.Utils (decapitalize, removePrefix, removeOptionalPrefix)
#endif
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Vector.Vector2 (Vector2)
#ifndef NO_CODE
import qualified Data.Char as Char
#endif
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as Expression
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Graphics.DrawingCombinators as Draw
import           Lamdu.Config.Theme.Name as Exported
import           Lamdu.Config.Theme.TextColors as Exported (TextColors(..))
import           Lamdu.Config.Theme.ValAnnotation as Exported (ValAnnotation(..))
import           Lamdu.Font (FontSize, Fonts)
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

import           Lamdu.Prelude

data Help = Help
    { helpTextSize :: FontSize
    , helpTextColor :: Draw.Color
    , helpInputDocColor :: Draw.Color
    , helpBGColor :: Draw.Color
    , helpTint :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "help"}
#endif
    ''Help

data Hole = Hole
    { holeResultPadding :: Vector2 Double
    , holeSearchTermBGColor :: Draw.Color
    , holeActiveSearchTermBGColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "hole"}
#endif
    ''Hole

data Eval = Eval
    { neighborsScaleFactor :: Vector2 Double
    , neighborsPadding :: Vector2 Double
    , staleResultTint :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Eval

data ToolTip = ToolTip
    { tooltipFgColor :: Draw.Color
    , tooltipBgColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "tooltip"}
#endif
    ''ToolTip

data StatusBar = StatusBar
    { statusBarBGColor :: Draw.Color
    , statusBarHSpaces :: Double
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier =
     \name ->
     name
     & removePrefix "statusBar"
     & Lens.taking 2 traverse %~ Char.toLower
    }
#endif
    ''StatusBar

data Theme = Theme
    { fonts :: Fonts FilePath
    , baseTextSize :: FontSize
    , animationTimePeriodSec :: Double
    , animationRemainInPeriod :: Double
    , help :: Help
    , hole :: Hole
    , _menu :: Menu.Style
    , name :: Name
    , eval :: Eval
    , _hover :: Hover.Style
    , tooltip :: ToolTip
    , textColors :: TextColors
    , topPadding :: Draw.R
    , statusBar :: StatusBar
    , maxEvalViewSize :: Int
    , _versionControl :: VersionControl.Theme
    , valAnnotation :: ValAnnotation
    , _indent :: Expression.Style
    , backgroundColor :: Draw.Color
    , invalidCursorBGColor :: Draw.Color
    , typeIndicatorErrorColor :: Draw.Color
    , typeIndicatorMatchColor :: Draw.Color
    , typeIndicatorFrameWidth :: Vector2 Double
    , letItemPadding :: Vector2 Double
    , narrowUnderlineWidth :: Double
    , wideUnderlineWidth :: Double
    , valFrameBGColor :: Draw.Color
    , valFramePadding :: Vector2 Double
    , typeFrameBGColor :: Draw.Color
    , _stdSpacing :: Vector2 Double -- as ratio of space character size
    , cursorColor :: Draw.Color
    , cursorDecayExponent :: Draw.R
    , disabledColor :: Draw.Color
    , presentationChoiceScaleFactor :: Vector2 Double
    , evaluatedPathBGColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalPrefix "_"}
#endif
    ''Theme

Lens.makeLenses ''Theme

class HasTheme env where theme :: Lens' env Theme
instance HasTheme Theme where theme = id

instance Expression.HasStyle Theme where style = indent
instance Hover.HasStyle Theme where style = hover
