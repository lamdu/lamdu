{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
-- | The themes/ config format
module Lamdu.Config.Theme
    ( Help(..), Hole(..), Name(..), Eval(..), ValAnnotation(..)
    , Theme(..), themeStdSpacing
    , HasTheme(..)
    ) where

import qualified Data.Aeson.Types as Aeson
import           Data.Vector.Vector2 (Vector2)
import           GHC.Generics (Generic)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Responsive.Expression as Expression
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
    , holeResultInjectedScaleExponent :: Double
    , holeExtraSymbolColorUnselected :: Draw.Color
    , holeExtraSymbolColorSelected :: Draw.Color
    , holeSearchTermBGColor :: Draw.Color
    , holeActiveSearchTermBGColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Hole where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Hole

data Name = Name
    { collisionSuffixTextColor :: Draw.Color
    , collisionSuffixBGColor :: Draw.Color
    , collisionSuffixScaleFactor :: Vector2 Double
    , definitionColor :: Draw.Color
    , parameterColor :: Draw.Color
    , letColor :: Draw.Color
    , recordTagColor :: Draw.Color
    , caseTagColor :: Draw.Color
    , paramTagColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Name where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Name

data Eval = Eval
    { neighborsScaleFactor :: Vector2 Double
    , neighborsPadding :: Vector2 Double
    , staleResultTint :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Eval where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Eval

data ValAnnotation = ValAnnotation
    { valAnnotationBGColor :: Draw.Color
    , valAnnotationHoverBGColor :: Draw.Color
    , valAnnotationSpacing :: Double -- as ratio of line height
    , valAnnotationWidthExpansionLimit :: Double
    , valAnnotationShrinkAtLeast :: Double
    , valAnnotationMaxHeight :: Double
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON ValAnnotation where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON ValAnnotation

data Theme = Theme
    { fonts :: Fonts FilePath
    , baseTextSize :: FontSize
    , animationTimePeriodSec :: Double
    , animationRemainInPeriod :: Double
    , help :: Help
    , hole :: Hole
    , name :: Name
    , eval :: Eval
    , newDefinitionActionColor :: Draw.Color
    , topPadding :: Draw.R
    , maxEvalViewSize :: Int
    , versionControl :: VersionControl.Theme
    , valAnnotation :: ValAnnotation
    , indent :: Expression.Style
    , hoverBGColor :: Draw.Color
    , hoverDarkPadding :: Vector2 Double
    , hoverDarkBGColor :: Draw.Color
    , backgroundColor :: Draw.Color
    , baseColor :: Draw.Color
    , invalidCursorBGColor :: Draw.Color
    , nomColor :: Draw.Color
    , literalColor :: Draw.Color
    , typeIndicatorErrorColor :: Draw.Color
    , typeIndicatorMatchColor :: Draw.Color
    , typeIndicatorFrameWidth :: Vector2 Double
    , foreignModuleColor :: Draw.Color
    , foreignVarColor :: Draw.Color
    , letItemPadding :: Vector2 Double
    , underlineWidth :: Double
    , typeTint :: Draw.Color
    , valFrameBGColor :: Draw.Color
    , valFramePadding :: Vector2 Double
    , typeFrameBGColor :: Draw.Color
    , stdSpacing :: Vector2 Double -- as ratio of space character size
    , cursorBGColor :: Draw.Color
    , grammarColor :: Draw.Color
    , disabledColor :: Draw.Color
    , lightLambdaUnderlineColor :: Draw.Color
    , presentationChoiceScaleFactor :: Vector2 Double
    , evaluatedPathBGColor :: Draw.Color
    , presentationChoiceColor :: Draw.Color
    , caseTailColor :: Draw.Color
    , recordTailColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Theme where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Theme

class HasTheme env where theme :: Lens' env Theme
instance HasTheme Theme where theme = id

themeStdSpacing :: Lens' Theme (Vector2 Double)
themeStdSpacing f t = stdSpacing t & f <&> \new -> t { stdSpacing = new }
