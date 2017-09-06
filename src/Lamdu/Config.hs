{-# OPTIONS -O0 #-}
{-# LANGUAGE DeriveGeneric, NoImplicitPrelude #-}
module Lamdu.Config
    ( Export(..), Pane(..), Hole(..)
    , Eval(..)
    , LiteralText(..)
    , Config(..)
    , HasConfig(..)
    , delKeys
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

import           Lamdu.Prelude

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
    , paneMoveDownKeys :: [MetaKey]
    , paneMoveUpKeys :: [MetaKey]
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
    { zoom :: Zoom.Config
    , export :: Export
    , pane :: Pane
    , versionControl :: VersionControl.Config
    , hole :: Hole
    , literalText :: LiteralText
    , eval :: Eval

    , maxExprDepth :: Int

    , helpKeys :: [MetaKey]
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

    , letAddItemKeys :: [MetaKey]

    , extractKeys :: [MetaKey]
    , inlineKeys :: [MetaKey]
    , moveLetInwardKeys:: [MetaKey]

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

class HasConfig env where config :: Lens' env Config
instance HasConfig Config where config = id

delKeys :: (MonadReader env m, HasConfig env) => m [MetaKey]
delKeys = sequence [Lens.view config <&> delForwardKeys, Lens.view config <&> delBackwardKeys] <&> mconcat
