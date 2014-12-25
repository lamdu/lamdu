{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Config
  ( Layers(..)
  , Help(..)
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

data Config = Config
  { layers :: Layers
  , help :: Help

  , baseColor :: Draw.Color
  , baseTextSize :: Int
  , shrinkKeys :: [E.ModKey]
  , enlargeKeys :: [E.ModKey]
  , enlargeFactor :: Double
  , shrinkFactor :: Double


  , invalidCursorBGColor :: Draw.Color

  , quitKeys :: [E.ModKey]
  , undoKeys :: [E.ModKey]
  , redoKeys :: [E.ModKey]
  , makeBranchKeys :: [E.ModKey]

  , jumpToBranchesKeys :: [E.ModKey]

  , addNextParamKeys :: [E.ModKey]

  , delBranchKeys :: [E.ModKey]

  , closePaneKeys :: [E.ModKey]
  , movePaneDownKeys :: [E.ModKey]
  , movePaneUpKeys :: [E.ModKey]

  , replaceKeys :: [E.ModKey]

  , pickResultKeys :: [E.ModKey]
  , pickAndMoveToNextHoleKeys :: [E.ModKey]

  , jumpToNextHoleKeys :: [E.ModKey]
  , jumpToPrevHoleKeys :: [E.ModKey]

  , jumpToDefinitionKeys :: [E.ModKey]

  , delForwardKeys :: [E.ModKey]
  , delBackwardKeys :: [E.ModKey]
  , wrapKeys :: [E.ModKey]
  , recordOpenKeys :: [E.ModKey]
  , debugModeKeys :: [E.ModKey]

  , newDefinitionKeys :: [E.ModKey]

  , literalIntColor :: Draw.Color

  , previousCursorKeys :: [E.ModKey]

  , holeResultCount :: Int
  , holeResultScaleFactor :: Vector2 Double
  , holeResultPadding :: Vector2 Double
  , holeResultInjectedScaleExponent :: Double
  , holeSearchTermScaleFactor :: Vector2 Double
  , holeInactiveExtraSymbolColor :: Draw.Color

  , typeErrorColor :: Draw.Color
  , typeMatchColor :: Draw.Color
  , acceptFirstTypeColor :: Draw.Color

  , activeHoleBGColor :: Draw.Color
  , inactiveHoleBGColor :: Draw.Color

  , wrapperHoleFrameWidth :: Vector2 Double

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

  , inactiveTintColor :: Draw.Color
  , activeDefBGColor :: Draw.Color

  , typeTint :: Draw.Color
  , typeBoxBGColor :: Draw.Color

  , autoNameOriginFGColor :: Draw.Color
  , nameOriginFGColor :: Draw.Color
  , definitionColor :: Draw.Color
  , parameterColor :: Draw.Color

  , cursorBGColor :: Draw.Color

  , grammarColor :: Draw.Color

  , listAddItemKeys :: [E.ModKey]

  , selectedBranchColor :: Draw.Color

  , jumpLHStoRHSKeys :: [E.ModKey]
  , jumpRHStoLHSKeys :: [E.ModKey]

  , acceptKeys :: [E.ModKey]

  , collisionSuffixTextColor :: Draw.Color
  , collisionSuffixBGColor :: Draw.Color
  , collisionSuffixScaleFactor :: Vector2 Double

  , paramDefSuffixScaleFactor :: Vector2 Double

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

instance ToJSON Config
instance FromJSON Config
