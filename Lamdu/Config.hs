{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric, NoImplicitPrelude #-}
module Lamdu.Config
    ( Help(..), Zoom(..), Export(..), Pane(..), Hole(..)
    , Eval(..)
    , LiteralText(..)
    , Config(..)
    , delKeys
    ) where

import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import           Graphics.UI.Bottle.ModKey (ModKey)
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import           Lamdu.Prelude

data Help = Help
    { helpKeys :: [ModKey]
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
    { paneCloseKeys :: [ModKey]
    , newDefinitionKeys :: [ModKey]
    , newDefinitionButtonPressKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Pane where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Pane

data Hole = Hole
    { holePickAndMoveToNextHoleKeys :: [ModKey]
    , holeJumpToNextKeys :: [ModKey]
    , holeJumpToPrevKeys :: [ModKey]
    , holeResultCount :: Int
    , holePickResultKeys :: [ModKey]
    , holeUnwrapKeys :: [ModKey]
    , holeOpenKeys :: [ModKey]
    , holeCloseKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Hole where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Hole

data Eval = Eval
    { prevScopeKeys :: [ModKey]
    , nextScopeKeys :: [ModKey]
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

data Config = Config
    { help :: Help
    , zoom :: Zoom
    , export :: Export
    , pane :: Pane
    , versionControl :: VersionControl.Config
    , hole :: Hole
    , literalText :: LiteralText
    , eval :: Eval
    , theme :: Text

    , maxExprDepth :: Int

    , quitKeys :: [ModKey]
    , nextInfoModeKeys :: [ModKey]
    , previousCursorKeys :: [ModKey]

    , addNextParamKeys :: [ModKey]
    , paramOrderBeforeKeys :: [ModKey]
    , paramOrderAfterKeys :: [ModKey]
    , jumpToDefinitionKeys :: [ModKey]
    , delForwardKeys :: [ModKey]
    , delBackwardKeys :: [ModKey]
    , wrapKeys :: [ModKey]

    , acceptDefinitionTypeKeys :: [ModKey]

    , letAddItemKeys :: [ModKey]

    , extractKeys :: [ModKey]
    , inlineKeys :: [ModKey]
    , moveLetInwardKeys:: [ModKey]

    , jumpLHStoRHSKeys :: [ModKey]
    , jumpRHStoLHSKeys :: [ModKey]

    , enterSubexpressionKeys :: [ModKey]
    , leaveSubexpressionKeys :: [ModKey]

    , recordOpenKeys :: [ModKey]
    , recordAddFieldKeys :: [ModKey]

    , caseOpenKeys :: [ModKey]
    , caseAddAltKeys :: [ModKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Config where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Config

delKeys :: Config -> [ModKey]
delKeys config = delForwardKeys config ++ delBackwardKeys config
