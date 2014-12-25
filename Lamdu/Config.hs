{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Config
  ( Layers(..)
  , Help(..), Zoom(..), Pane(..), VersionControl(..), Hole(..), Name(..)
  , Config(..)
  , delKeys
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Vector.Vector2 (Vector2(..))
import GHC.Generics (Generic)
import Graphics.DrawingCombinators.Utils () -- Read instance for Color
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E

data Layers = Layers
  { layerCursorBG
  , layerTypes
  , layerChoiceBG
  , layerHoleBG
  , layerNameCollisionBG
  , layerValFrameBG
  , layerParensHighlightBG
  , layerActivePane
  , layerMax :: Int
  } deriving (Eq, Generic, Show)

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
  , holeWrapperFrameWidth :: Vector2 Double
  , holePickResultKeys :: [E.ModKey]
  } deriving (Eq, Generic, Show)

data Name = Name
  { collisionSuffixTextColor :: Draw.Color
  , collisionSuffixBGColor :: Draw.Color
  , collisionSuffixScaleFactor :: Vector2 Double
  , autoNameOriginFGColor :: Draw.Color
  , nameOriginFGColor :: Draw.Color
  , definitionColor :: Draw.Color
  , parameterColor :: Draw.Color
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

  , invalidCursorBGColor :: Draw.Color

  , quitKeys :: [E.ModKey]

  , addNextParamKeys :: [E.ModKey]

  , replaceKeys :: [E.ModKey]

  , jumpToDefinitionKeys :: [E.ModKey]

  , delForwardKeys :: [E.ModKey]
  , delBackwardKeys :: [E.ModKey]
  , wrapKeys :: [E.ModKey]
  , recordOpenKeys :: [E.ModKey]
  , debugModeKeys :: [E.ModKey]

  , newDefinitionKeys :: [E.ModKey]

  , literalIntColor :: Draw.Color

  , previousCursorKeys :: [E.ModKey]

  , typeErrorColor :: Draw.Color
  , typeMatchColor :: Draw.Color
  , acceptTypeForFirstTimeColor :: Draw.Color

  , tagScaleFactor :: Vector2 Double
  , fieldTagScaleFactor :: Vector2 Double
  , fieldTint :: Draw.Color

  , suggestedValueScaleFactor :: Vector2 Double
  , suggestedValueTint :: Draw.Color

  , parenHighlightColor :: Draw.Color

  , addWhereItemKeys :: [E.ModKey]

  , whereScaleFactor :: Vector2 Double
  , whereLabelScaleFactor :: Vector2 Double

  , typeScaleFactor :: Vector2 Double

  , foreignModuleColor :: Draw.Color
  , foreignVarColor :: Draw.Color

  , cutKeys :: [E.ModKey]
  , pasteKeys :: [E.ModKey]

  , typeTint :: Draw.Color
  , typeBoxBGColor :: Draw.Color

  , cursorBGColor :: Draw.Color

  , grammarColor :: Draw.Color

  , listAddItemKeys :: [E.ModKey]

  , jumpLHStoRHSKeys :: [E.ModKey]
  , jumpRHStoLHSKeys :: [E.ModKey]

  , acceptKeys :: [E.ModKey]

  , enterSubexpressionKeys :: [E.ModKey]
  , leaveSubexpressionKeys :: [E.ModKey]

  , nextInfoModeKeys :: [E.ModKey]

  , recordTailColor :: Draw.Color
  , recordAddFieldKeys :: [E.ModKey]

  , presentationChoiceScaleFactor :: Vector2 Double
  , presentationChoiceColor :: Draw.Color

  , valFrameBGColor :: Draw.Color
  , valFramePadding :: Vector2 Double
  , typeFrameBGColor :: Draw.Color
  , verticalSpacing :: Double
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
