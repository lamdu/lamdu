{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Lamdu.Config
  ( Layers(..)
  , Help(..), Zoom(..), Pane(..), VersionControl(..), Hole(..), Name(..)
  , Config(..)
  , delKeys
  , layerInterval
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Vector.Vector2 (Vector2(..))
import GHC.Generics (Generic)
import Graphics.DrawingCombinators.Utils () -- Read instance for Color
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E

data Layers = Layers
  { layerMin
  , layerCursor
  , layerTypes
  , layerChoiceBG
  , layerHoleBG
  , layerDarkActiveHoleBG
  , layerNameCollisionBG
  , layerValFrameBG
  , layerParensHighlightBG
  , layerActivePane
  , layerMax :: Int
  } deriving (Eq, Generic, Show)

layerInterval :: Layers -> Int
layerInterval Layers{..} = layerMax - layerMin

data Help = Help
  { helpTextColor :: Draw.Color
  , helpTextSize :: Int
  , helpInputDocColor :: Draw.Color
  , helpBGColor :: Draw.Color
  , helpKeys :: [E.ModKey]
  } deriving (Eq, Generic, Show)

data Zoom = Zoom
  { shrinkKeys :: [E.ModKey]
  , enlargeKeys :: [E.ModKey]
  , enlargeFactor :: Double
  , shrinkFactor :: Double
  } deriving (Eq, Generic, Show)

data Pane = Pane
  { paneInactiveTintColor :: Draw.Color
  , paneActiveBGColor :: Draw.Color
  , paneCloseKeys :: [E.ModKey]
  , paneMoveDownKeys :: [E.ModKey]
  , paneMoveUpKeys :: [E.ModKey]
  , newDefinitionKeys :: [E.ModKey]
  } deriving (Eq, Generic, Show)

data VersionControl = VersionControl
  { undoKeys :: [E.ModKey]
  , redoKeys :: [E.ModKey]
  , makeBranchKeys :: [E.ModKey]
  , jumpToBranchesKeys :: [E.ModKey]
  , delBranchKeys :: [E.ModKey]
  , selectedBranchColor :: Draw.Color
  } deriving (Eq, Generic, Show)

data Hole = Hole
  { holePickAndMoveToNextHoleKeys :: [E.ModKey]
  , holeJumpToNextKeys :: [E.ModKey]
  , holeJumpToPrevKeys :: [E.ModKey]
  , holeResultCount :: Int
  , holeResultScaleFactor :: Vector2 Double
  , holeResultPadding :: Vector2 Double
  , holeResultInjectedScaleExponent :: Double
  , holeSearchTermScaleFactor :: Vector2 Double
  , holeInactiveExtraSymbolColor :: Draw.Color
  , holeActiveBGColor :: Draw.Color
  , holeInactiveBGColor :: Draw.Color
  , holePickResultKeys :: [E.ModKey]
  , holeActiveDarkPadding :: Vector2 Double
  , holeActiveDarkBGColor :: Draw.Color
  , holeUnwrapKeys :: [E.ModKey]
  , holeOpenKeys :: [E.ModKey]
  } deriving (Eq, Generic, Show)

data Name = Name
  { collisionSuffixTextColor :: Draw.Color
  , collisionSuffixBGColor :: Draw.Color
  , collisionSuffixScaleFactor :: Vector2 Double
  , autoNameOriginFGColor :: Draw.Color
  , nameOriginFGColor :: Draw.Color
  , definitionColor :: Draw.Color
  , parameterColor :: Draw.Color
  , recordTagColor :: Draw.Color
  , recordTagScaleFactor :: Vector2 Double
  , paramTagColor :: Draw.Color
  , paramTagScaleFactor :: Vector2 Double
  } deriving (Eq, Generic, Show)

data Config = Config
  { layers :: Layers
  , help :: Help
  , zoom :: Zoom
  , pane :: Pane
  , versionControl :: VersionControl
  , hole :: Hole
  , name :: Name

  , baseColor :: Draw.Color
  , baseTextSize :: Int

  , quitKeys :: [E.ModKey]
  , debugModeKeys :: [E.ModKey]
  , nextInfoModeKeys :: [E.ModKey]
  , previousCursorKeys :: [E.ModKey]

  , invalidCursorBGColor :: Draw.Color

  , addNextParamKeys :: [E.ModKey]
  , jumpToDefinitionKeys :: [E.ModKey]
  , replaceKeys :: [E.ModKey]
  , delForwardKeys :: [E.ModKey]
  , delBackwardKeys :: [E.ModKey]
  , wrapKeys :: [E.ModKey]

  , parenHighlightColor :: Draw.Color
  , literalIntColor :: Draw.Color
  , typeIndicatorErrorColor :: Draw.Color
  , typeIndicatorMatchColor :: Draw.Color
  , typeIndicatorFrameWidth :: Vector2 Double
  , foreignModuleColor :: Draw.Color
  , foreignVarColor :: Draw.Color

  , acceptDefinitionTypeForFirstTimeColor :: Draw.Color
  , acceptDefinitionTypeKeys :: [E.ModKey]

  , suggestedValueScaleFactor :: Vector2 Double
  , suggestedValueTint :: Draw.Color

  , whereAddItemKeys :: [E.ModKey]
  , whereItemPadding :: Vector2 Double

  , typeScaleFactor :: Vector2 Double

  , cutKeys :: [E.ModKey]
  , pasteKeys :: [E.ModKey]

  , typeTint :: Draw.Color
  , typeBoxBGColor :: Draw.Color
  , valFrameBGColor :: Draw.Color
  , valFramePadding :: Vector2 Double
  , valInferredSpacing :: Double
  , typeFrameBGColor :: Draw.Color
  , verticalSpacing :: Double
  , cursorBGColor :: Draw.Color
  , grammarColor :: Draw.Color

  , listAddItemKeys :: [E.ModKey]

  , jumpLHStoRHSKeys :: [E.ModKey]
  , jumpRHStoLHSKeys :: [E.ModKey]

  , enterSubexpressionKeys :: [E.ModKey]
  , leaveSubexpressionKeys :: [E.ModKey]

  , recordOpenKeys :: [E.ModKey]
  , recordTailColor :: Draw.Color
  , recordAddFieldKeys :: [E.ModKey]

  , presentationChoiceScaleFactor :: Vector2 Double
  , presentationChoiceColor :: Draw.Color
  } deriving (Eq, Generic, Show)

delKeys :: Config -> [E.ModKey]
delKeys config = delForwardKeys config ++ delBackwardKeys config

instance ToJSON Layers
instance FromJSON Layers

instance ToJSON Help
instance FromJSON Help

instance ToJSON Zoom
instance FromJSON Zoom

instance ToJSON Pane
instance FromJSON Pane

instance ToJSON VersionControl
instance FromJSON VersionControl

instance ToJSON Hole
instance FromJSON Hole

instance ToJSON Name
instance FromJSON Name

instance ToJSON Config
instance FromJSON Config
