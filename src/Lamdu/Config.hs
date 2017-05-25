{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric, NoImplicitPrelude #-}
module Lamdu.Config
    ( Help(..), Export(..), Pane(..), Hole(..)
    , Eval(..)
    , LiteralText(..)
    , Config(..)
    , delKeys
    ) where

import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import           Graphics.UI.Bottle.MetaKey (MetaKey)
import qualified Graphics.UI.Bottle.Zoom as Zoom
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import           Lamdu.Prelude

data Help = Help
    { helpKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Help where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Help

data Export = Export
    { exportPath :: FilePath
    , exportKeys :: [MetaKey]
    , exportFancyKeys :: [MetaKey]
    , exportAllKeys :: [MetaKey]
    , importKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Export where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Export

data Pane = Pane
    { paneCloseKeys :: [MetaKey]
    , newDefinitionKeys :: [MetaKey]
    , newDefinitionButtonPressKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Pane where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Pane

data Hole = Hole
    { holePickAndMoveToNextHoleKeys :: [MetaKey]
    , holeJumpToNextKeys :: [MetaKey]
    , holeJumpToPrevKeys :: [MetaKey]
    , holeResultCount :: Int
    , holePickResultKeys :: [MetaKey]
    , holeUnwrapKeys :: [MetaKey]
    , holeOpenKeys :: [MetaKey]
    , holeCloseKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Hole where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Hole

data Eval = Eval
    { prevScopeKeys :: [MetaKey]
    , nextScopeKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Eval where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Eval

data LiteralText = LiteralText
    { literalTextStartEditingKeys :: [MetaKey]
    , literalTextStopEditingKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON LiteralText where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON LiteralText

data Config = Config
    { help :: Help
    , zoom :: Zoom.Config
    , export :: Export
    , pane :: Pane
    , versionControl :: VersionControl.Config
    , hole :: Hole
    , literalText :: LiteralText
    , eval :: Eval

    , maxExprDepth :: Int

    , quitKeys :: [MetaKey]
    , changeThemeKeys :: [MetaKey]
    , nextInfoModeKeys :: [MetaKey]
    , previousCursorKeys :: [MetaKey]

    , addNextParamKeys :: [MetaKey]
    , paramOrderBeforeKeys :: [MetaKey]
    , paramOrderAfterKeys :: [MetaKey]
    , jumpToDefinitionKeys :: [MetaKey]
    , delForwardKeys :: [MetaKey]
    , delBackwardKeys :: [MetaKey]
    , replaceParentKeys :: [MetaKey]
    , wrapKeys :: [MetaKey]

    , acceptDefinitionTypeKeys :: [MetaKey]

    , letAddItemKeys :: [MetaKey]

    , extractKeys :: [MetaKey]
    , inlineKeys :: [MetaKey]
    , moveLetInwardKeys:: [MetaKey]

    , jumpLHStoRHSKeys :: [MetaKey]
    , jumpRHStoLHSKeys :: [MetaKey]

    , enterSubexpressionKeys :: [MetaKey]
    , leaveSubexpressionKeys :: [MetaKey]

    , recordOpenKeys :: [MetaKey]
    , recordAddFieldKeys :: [MetaKey]

    , caseOpenKeys :: [MetaKey]
    , caseAddAltKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Config where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Config

delKeys :: Config -> [MetaKey]
delKeys config = delForwardKeys config ++ delBackwardKeys config
