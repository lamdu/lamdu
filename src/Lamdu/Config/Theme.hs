{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
-- | The themes/ config format
module Lamdu.Config.Theme
    ( Help(..), helpTextSize, helpTextColor, helpInputDocColor, helpBGColor, helpTint
    , Hole(..), holeResultPadding, holeSearchTermBGColor, holeActiveSearchTermBGColor
    , Eval(..), neighborsScaleFactor, neighborsPadding, staleResultTint
    , ToolTip(..), tooltipFgColor, tooltipBgColor
    , StatusBar(..), statusBarBGColor, statusBarHSpaces
    , Theme(..)
        , fonts, baseTextSize, animationTimePeriodSec
        , animationRemainInPeriod, help, hole, menu, name, eval, hover, tooltip
        , textColors, topPadding, statusBar, maxEvalViewSize, versionControl
        , valAnnotation, indent, backgroundColor, invalidCursorBGColor
        , errorColor, successColor
        , typeIndicatorFrameWidth, letItemPadding, narrowUnderlineWidth
        , wideUnderlineWidth, valFrameBGColor, valFramePadding
        , typeFrameBGColor, stdSpacing, cursorColor, cursorDecayExponent
        , disabledColor, presentationChoiceScaleFactor, evaluatedPathBGColor
    , HasTheme(..)
    ) where

import qualified Control.Lens as Lens
#ifndef NO_CODE
import           Data.Aeson.Utils (decapitalize, removePrefix)
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
import           Lamdu.Config.Theme.Name (Name(..))
import           Lamdu.Config.Theme.TextColors (TextColors(..))
import           Lamdu.Config.Theme.ValAnnotation (ValAnnotation(..))
import           Lamdu.Font (FontSize, Fonts)
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

import           Lamdu.Prelude

data Help = Help
    { _helpTextSize :: FontSize
    , _helpTextColor :: Draw.Color
    , _helpInputDocColor :: Draw.Color
    , _helpBGColor :: Draw.Color
    , _helpTint :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "_help"}
#endif
    ''Help

Lens.makeLenses ''Help

data Hole = Hole
    { _holeResultPadding :: Vector2 Double
    , _holeSearchTermBGColor :: Draw.Color
    , _holeActiveSearchTermBGColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "_hole"}
#endif
    ''Hole

Lens.makeLenses ''Hole

data Eval = Eval
    { _neighborsScaleFactor :: Vector2 Double
    , _neighborsPadding :: Vector2 Double
    , _staleResultTint :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removePrefix "_"}
#endif
    ''Eval

Lens.makeLenses ''Eval

data ToolTip = ToolTip
    { _tooltipFgColor :: Draw.Color
    , _tooltipBgColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "_tooltip"}
#endif
    ''ToolTip

Lens.makeLenses ''ToolTip

data StatusBar = StatusBar
    { _statusBarBGColor :: Draw.Color
    , _statusBarHSpaces :: Double
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier =
     \name ->
     name
     & removePrefix "_statusBar"
     & Lens.taking 2 traverse %~ Char.toLower
    }
#endif
    ''StatusBar

Lens.makeLenses ''StatusBar

data Theme = Theme
    { _fonts :: Fonts FilePath
    , _baseTextSize :: FontSize
    , _animationTimePeriodSec :: Double
    , _animationRemainInPeriod :: Double
    , _help :: Help
    , _hole :: Hole
    , _menu :: Menu.Style
    , _name :: Name
    , _eval :: Eval
    , _hover :: Hover.Style
    , _tooltip :: ToolTip
    , _textColors :: TextColors
    , _topPadding :: Draw.R
    , _statusBar :: StatusBar
    , _maxEvalViewSize :: Int
    , _versionControl :: VersionControl.Theme
    , _valAnnotation :: ValAnnotation
    , _indent :: Expression.Style
    , _backgroundColor :: Draw.Color
    , _invalidCursorBGColor :: Draw.Color
    , _errorColor :: Draw.Color
    , _successColor :: Draw.Color
    , _typeIndicatorFrameWidth :: Vector2 Double
    , _letItemPadding :: Vector2 Double
    , _narrowUnderlineWidth :: Double
    , _wideUnderlineWidth :: Double
    , _valFrameBGColor :: Draw.Color
    , _valFramePadding :: Vector2 Double
    , _typeFrameBGColor :: Draw.Color
    , _stdSpacing :: Vector2 Double -- as ratio of space character size
    , _cursorColor :: Draw.Color
    , _cursorDecayExponent :: Draw.R
    , _disabledColor :: Draw.Color
    , _presentationChoiceScaleFactor :: Vector2 Double
    , _evaluatedPathBGColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removePrefix "_"}
#endif
    ''Theme

Lens.makeLenses ''Theme

class HasTheme env where theme :: Lens' env Theme
instance HasTheme Theme where theme = id

instance Expression.HasStyle Theme where style = indent
instance Hover.HasStyle Theme where style = hover
