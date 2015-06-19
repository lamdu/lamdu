{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Lamdu.Config
    ( Layers(..)
    , Help(..), Zoom(..), Pane(..), Hole(..), Name(..), Eval(..)
    , Config(..)
    , delKeys
    , layerInterval
    ) where

import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils ()
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

data Layers = Layers
    { layerMin
    , layerCursor
    , layerAnnotations
    , layerChoiceBG
    , layerHoleBG
    , layerDarkHoleBG
    , layerNameCollisionBG
    , layerValFrameBG
    , layerParensHighlightBG
    , layerActivePane
    , layerMax :: Anim.Layer
    } deriving (Eq, Generic, Show)
instance ToJSON Layers
instance FromJSON Layers

layerInterval :: Layers -> Int
layerInterval Layers{..} = layerMax - layerMin

data Help = Help
    { helpTextColor :: Draw.Color
    , helpTextSize :: Double
    , helpInputDocColor :: Draw.Color
    , helpBGColor :: Draw.Color
    , helpKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance ToJSON Help
instance FromJSON Help

data Zoom = Zoom
    { shrinkKeys :: [ModKey]
    , enlargeKeys :: [ModKey]
    , enlargeFactor :: Double
    , shrinkFactor :: Double
    } deriving (Eq, Generic, Show)
instance ToJSON Zoom
instance FromJSON Zoom

data Pane = Pane
    { paneInactiveTintColor :: Draw.Color
    , paneActiveBGColor :: Draw.Color
    , paneCloseKeys :: [ModKey]
    , paneMoveDownKeys :: [ModKey]
    , paneMoveUpKeys :: [ModKey]
    , -- Need some padding on top because of on-top hovers, this decides
        -- how much:
        paneHoverPadding :: Draw.R
    , newDefinitionKeys :: [ModKey]
    , newDefinitionActionColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance ToJSON Pane
instance FromJSON Pane

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
    , holePasteKeys :: [ModKey]
    , holeHoveringWrapperScaleFactor :: Vector2 Double
    } deriving (Eq, Generic, Show)
instance ToJSON Hole
instance FromJSON Hole

data Name = Name
    { collisionSuffixTextColor :: Draw.Color
    , collisionSuffixBGColor :: Draw.Color
    , collisionSuffixScaleFactor :: Vector2 Double
    , autoNameOriginFGColor :: Draw.Color
    , nameOriginFGColor :: Draw.Color
    , definitionColor :: Draw.Color
    , parameterColor :: Draw.Color
    , recordTagColor :: Draw.Color
    , paramTagColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance ToJSON Name
instance FromJSON Name

data Eval = Eval
    { prevScopeKeys :: [ModKey]
    , nextScopeKeys :: [ModKey]
    , neighborsScaleFactor :: Vector2 Double
    } deriving (Eq, Generic, Show)
instance ToJSON Eval
instance FromJSON Eval

data Config = Config
    { layers :: Layers
    , help :: Help
    , zoom :: Zoom
    , pane :: Pane
    , versionControl :: VersionControl.Config
    , hole :: Hole
    , name :: Name
    , eval :: Eval

    , animationTimePeriodSec :: Double
    , animationRemainInPeriod :: Double

    , backgroundColor :: Draw.Color
    , baseColor :: Draw.Color
    , baseTextSize :: Double
    , spaceWidth :: Double

    , quitKeys :: [ModKey]
    , debugModeKeys :: [ModKey]
    , nextInfoModeKeys :: [ModKey]
    , previousCursorKeys :: [ModKey]

    , invalidCursorBGColor :: Draw.Color

    , addNextParamKeys :: [ModKey]
    , jumpToDefinitionKeys :: [ModKey]
    , replaceKeys :: [ModKey]
    , delForwardKeys :: [ModKey]
    , delBackwardKeys :: [ModKey]
    , wrapKeys :: [ModKey]

    , parenHighlightColor :: Draw.Color
    , literalIntColor :: Draw.Color
    , typeIndicatorErrorColor :: Draw.Color
    , typeIndicatorMatchColor :: Draw.Color
    , typeIndicatorFirstTimeColor :: Draw.Color
    , typeIndicatorFrameWidth :: Vector2 Double
    , foreignModuleColor :: Draw.Color
    , foreignVarColor :: Draw.Color

    , acceptDefinitionTypeKeys :: [ModKey]

    , whereAddItemKeys :: [ModKey]
    , whereItemPadding :: Vector2 Double

    , cutKeys :: [ModKey]

    , typeTint :: Draw.Color
    , typeBoxBGColor :: Draw.Color
    , valFrameBGColor :: Draw.Color
    , valFramePadding :: Vector2 Double
    , valAnnotationSpacing :: Double
    , typeFrameBGColor :: Draw.Color
    , verticalSpacing :: Double
    , cursorBGColor :: Draw.Color
    , grammarColor :: Draw.Color

    , listAddItemKeys :: [ModKey]

    , jumpLHStoRHSKeys :: [ModKey]
    , jumpRHStoLHSKeys :: [ModKey]

    , enterSubexpressionKeys :: [ModKey]
    , leaveSubexpressionKeys :: [ModKey]

    , recordOpenKeys :: [ModKey]
    , recordTailColor :: Draw.Color
    , recordAddFieldKeys :: [ModKey]

    , presentationChoiceScaleFactor :: Vector2 Double
    , presentationChoiceColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance ToJSON Config
instance FromJSON Config

delKeys :: Config -> [ModKey]
delKeys config = delForwardKeys config ++ delBackwardKeys config
