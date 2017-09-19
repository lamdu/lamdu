{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
-- | The themes/ config format
module Lamdu.Config.Theme
    ( module Lamdu.Config.Theme.CodeForegroundColors
    , module Lamdu.Config.Theme.Name
    , module Lamdu.Config.Theme.ValAnnotation
    , Help(..), Hole(..), Eval(..)
    , Theme(..), themeStdSpacing
    , HasTheme(..)
    ) where

import qualified Data.Aeson.Types as Aeson
import           Data.Vector.Vector2 (Vector2)
import           GHC.Generics (Generic)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as Expression
import qualified GUI.Momentu.Widgets.Menu as Menu
import           Lamdu.Config.Theme.CodeForegroundColors (CodeForegroundColors(..))
import           Lamdu.Config.Theme.Name (Name(..))
import           Lamdu.Config.Theme.ValAnnotation (ValAnnotation(..))
import           Lamdu.Font (FontSize, Fonts)
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

import           Lamdu.Prelude

data Help = Help
    { helpTextSize :: FontSize
    , helpTextColor :: Draw.Color
    , helpInputDocColor :: Draw.Color
    , helpBGColor :: Draw.Color
    , helpTint :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Help where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Help

data Hole = Hole
    { holeResultPadding :: Vector2 Double
    , holeSearchTermBGColor :: Draw.Color
    , holeActiveSearchTermBGColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Hole where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Hole

data Eval = Eval
    { neighborsScaleFactor :: Vector2 Double
    , neighborsPadding :: Vector2 Double
    , staleResultTint :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Eval where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Eval

data Theme = Theme
    { fonts :: Fonts FilePath
    , baseTextSize :: FontSize
    , animationTimePeriodSec :: Double
    , animationRemainInPeriod :: Double
    , help :: Help
    , hole :: Hole
    , menu :: Menu.Style
    , name :: Name
    , eval :: Eval
    , hover :: Hover.Style
    , codeForegroundColors :: CodeForegroundColors
    , newDefinitionActionColor :: Draw.Color
    , topPadding :: Draw.R
    , maxEvalViewSize :: Int
    , versionControl :: VersionControl.Theme
    , valAnnotation :: ValAnnotation
    , indent :: Expression.Style
    , backgroundColor :: Draw.Color
    , invalidCursorBGColor :: Draw.Color
    , typeIndicatorErrorColor :: Draw.Color
    , typeIndicatorMatchColor :: Draw.Color
    , typeIndicatorFrameWidth :: Vector2 Double
    , letItemPadding :: Vector2 Double
    , underlineWidth :: Double
    , typeTint :: Draw.Color
    , valFrameBGColor :: Draw.Color
    , valFramePadding :: Vector2 Double
    , typeFrameBGColor :: Draw.Color
    , stdSpacing :: Vector2 Double -- as ratio of space character size
    , cursorBGColor :: Draw.Color
    , disabledColor :: Draw.Color
    , presentationChoiceScaleFactor :: Vector2 Double
    , evaluatedPathBGColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Theme where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Theme

class HasTheme env where theme :: Lens' env Theme
instance HasTheme Theme where theme = id

themeStdSpacing :: Lens' Theme (Vector2 Double)
themeStdSpacing f t = stdSpacing t & f <&> \new -> t { stdSpacing = new }

instance Expression.HasStyle Theme where style f t = f (indent t) <&> \x -> t { indent = x }
instance Menu.HasStyle Theme where style f t = f (menu t) <&> \x -> t { menu = x }
instance Hover.HasStyle Theme where style f t = f (hover t) <&> \x -> t { hover = x }
