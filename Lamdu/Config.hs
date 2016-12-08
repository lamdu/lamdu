{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Lamdu.Config
    ( Help(..), Zoom(..), Export(..), Pane(..), Hole(..)
    , Name(..), Eval(..), Indent(..)
    , LiteralText(..)
    , LightLambda(..)
    , Config(..)
    , delKeys
    ) where

import qualified Data.Aeson.Types as Aeson
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Lamdu.Font (FontSize, Fonts(..))
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

data Help = Help
    { helpTextSize :: FontSize
    , helpTextColor :: Draw.Color
    , helpInputDocColor :: Draw.Color
    , helpBGColor :: Draw.Color
    , helpKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Help where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Help

data Zoom = Zoom
    { shrinkKeys :: [ModKey]
    , enlargeKeys :: [ModKey]
    , enlargeFactor :: Double
    , shrinkFactor :: Double
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Zoom where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Zoom

data Export = Export
    { exportPath :: FilePath
    , exportKeys :: [ModKey]
    , exportFancyKeys :: [ModKey]
    , exportAllKeys :: [ModKey]
    , importKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Export where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Export

data Pane = Pane
    { paneInactiveTintColor :: Draw.Color
    , paneActiveBGColor :: Draw.Color
    , paneCloseKeys :: [ModKey]
    , paneMoveDownKeys :: [ModKey]
    , paneMoveUpKeys :: [ModKey]
    , -- Need some padding on top because of on-top hovers, this decides
      -- how much (as ratio of line height):
      paneHoverPadding :: Draw.R
    , newDefinitionKeys :: [ModKey]
    , newDefinitionButtonPressKeys :: [ModKey]
    , newDefinitionActionColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Pane where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Pane

data Hole = Hole
    { holePickAndMoveToNextHoleKeys :: [ModKey]
    , holeJumpToNextKeys :: [ModKey]
    , holeJumpToPrevKeys :: [ModKey]
    , holeResultCount :: Int
    , holeResultPadding :: Vector2 Double
    , holeResultInjectedScaleExponent :: Double
    , holeExtraSymbolColorUnselected :: Draw.Color
    , holeExtraSymbolColorSelected :: Draw.Color
    , holeOpenBGColor :: Draw.Color
    , holeSearchTermBGColor :: Draw.Color
    , holePickResultKeys :: [ModKey]
    , holeDarkPadding :: Vector2 Double
    , holeDarkBGColor :: Draw.Color
    , holeUnwrapKeys :: [ModKey]
    , holeOpenKeys :: [ModKey]
    , holeCloseKeys :: [ModKey]
    , holeHoveringWrapperScaleFactor :: Vector2 Double
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Hole where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Hole

data Name = Name
    { collisionSuffixTextColor :: Draw.Color
    , collisionSuffixBGColor :: Draw.Color
    , collisionSuffixScaleFactor :: Vector2 Double
    , nameOriginFGColor :: Draw.Color
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

data LightLambda = LightLambda
    { lightLambdaUnderlineColor :: Draw.Color
    , lightLambdaUnderlineWidth :: Double
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON LightLambda where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON LightLambda

data Eval = Eval
    { prevScopeKeys :: [ModKey]
    , nextScopeKeys :: [ModKey]
    , neighborsScaleFactor :: Vector2 Double
    , neighborsPadding :: Vector2 Double
    , staleResultTint :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Eval where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Eval

data LiteralText = LiteralText
    { literalTextStartEditingKeys :: [ModKey]
    , literalTextStopEditingKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON LiteralText where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON LiteralText

data Indent = Indent
    { indentBarWidth :: Double
    , indentBarGap :: Double
    , indentBarColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Indent where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Indent

data Config = Config
    { fonts :: Fonts FilePath
    , baseTextSize :: FontSize
    , help :: Help
    , zoom :: Zoom
    , export :: Export
    , pane :: Pane
    , versionControl :: VersionControl.Config
    , hole :: Hole
    , literalText :: LiteralText
    , name :: Name
    , lightLambda :: LightLambda
    , eval :: Eval
    , indent :: Indent

    , animationTimePeriodSec :: Double
    , animationRemainInPeriod :: Double

    , maxExprDepth :: Int
    , maxEvalViewSize :: Int

    , backgroundColor :: Draw.Color
    , baseColor :: Draw.Color

    , quitKeys :: [ModKey]
    , nextInfoModeKeys :: [ModKey]
    , previousCursorKeys :: [ModKey]

    , invalidCursorBGColor :: Draw.Color

    , addNextParamKeys :: [ModKey]
    , paramOrderBeforeKeys :: [ModKey]
    , paramOrderAfterKeys :: [ModKey]
    , jumpToDefinitionKeys :: [ModKey]
    , delForwardKeys :: [ModKey]
    , delBackwardKeys :: [ModKey]
    , wrapKeys :: [ModKey]

    , literalColor :: Draw.Color
    , typeIndicatorErrorColor :: Draw.Color
    , typeIndicatorMatchColor :: Draw.Color
    , typeIndicatorFirstTimeColor :: Draw.Color
    , typeIndicatorFrameWidth :: Vector2 Double
    , foreignModuleColor :: Draw.Color
    , foreignVarColor :: Draw.Color

    , acceptDefinitionTypeKeys :: [ModKey]

    , letAddItemKeys :: [ModKey]
    , letItemPadding :: Vector2 Double

    , extractKeys :: [ModKey]
    , inlineKeys :: [ModKey]
    , moveLetInwardKeys:: [ModKey]

    , typeTint :: Draw.Color
    , valFrameBGColor :: Draw.Color
    , valFramePadding :: Vector2 Double
    , valNomBGColor :: Draw.Color
    , valAnnotationBGColor :: Draw.Color
    , valAnnotationHoverBGColor :: Draw.Color
    , valAnnotationSpacing :: Double -- as ratio of line height
    , valAnnotationWidthExpansionLimit :: Double
    , valAnnotationShrinkAtLeast :: Double
    , valAnnotationMaxHeight :: Double
    , typeFrameBGColor :: Draw.Color
    , stdSpacing :: Vector2 Double -- as ratio of space character size
    , cursorBGColor :: Draw.Color
    , grammarColor :: Draw.Color
    , disabledColor :: Draw.Color

    , jumpLHStoRHSKeys :: [ModKey]
    , jumpRHStoLHSKeys :: [ModKey]

    , enterSubexpressionKeys :: [ModKey]
    , leaveSubexpressionKeys :: [ModKey]

    , recordOpenKeys :: [ModKey]
    , recordTailColor :: Draw.Color
    , recordAddFieldKeys :: [ModKey]

    , caseOpenKeys :: [ModKey]
    , caseTailColor :: Draw.Color
    , caseAddAltKeys :: [ModKey]
    , evaluatedPathBGColor :: Draw.Color

    , presentationChoiceScaleFactor :: Vector2 Double
    , presentationChoiceColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Config where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Config

delKeys :: Config -> [ModKey]
delKeys config = delForwardKeys config ++ delBackwardKeys config
