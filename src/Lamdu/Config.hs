{-# OPTIONS -O0 #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, CPP #-}
module Lamdu.Config
    ( Export(..), Pane(..), Completion(..)
    , Debug(..)
    , Eval(..)
    , Literal(..)
    , Config(..)
    , HasConfig(..)
    , delKeys
    , menu
    , versionControl
    ) where

import qualified Control.Lens as Lens
#ifndef NO_CODE
import           Data.Aeson.Utils (removeOptionalUnderscore)
#endif
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Zoom as Zoom
import qualified Lamdu.GUI.VersionControl.Config as VersionControl

import           Lamdu.Prelude

data Export = Export
    { exportPath :: FilePath
    , exportKeys :: [MetaKey]
    , exportFancyKeys :: [MetaKey]
    , exportAllKeys :: [MetaKey]
    , executeKeys :: [MetaKey]
    , importKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Export

data Pane = Pane
    { paneCloseKeys :: [MetaKey]
    , paneMoveDownKeys :: [MetaKey]
    , paneMoveUpKeys :: [MetaKey]
    , newDefinitionKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Pane

data Completion = Completion
    { completionJumpToNextKeys :: [MetaKey]
    , completionJumpToPrevKeys :: [MetaKey]
    , completionResultCount :: Int
    , completionOpenKeys :: [MetaKey]
    , completionCloseKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Completion

data Eval = Eval
    { prevScopeKeys :: [MetaKey]
    , nextScopeKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Eval

data Literal = Literal
    { literalStartEditingKeys :: [MetaKey]
    , literalStopEditingKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Literal

data Debug = Debug
    { debugShowFPS :: Bool
    , virtualCursorShown :: Bool
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions ''Debug

data Config = Config
    { zoom :: Zoom.Config
    , export :: Export
    , pane :: Pane
    , _versionControl :: VersionControl.Config
    , completion :: Completion
    , literal :: Literal
    , eval :: Eval
    , debug :: Debug
    , _menu :: Menu.Keys

    , maxExprDepth :: Int

    , helpKeys :: [MetaKey]
    , quitKeys :: [MetaKey]
    , changeThemeKeys :: [MetaKey]
    , nextAnnotationModeKeys :: [MetaKey]
    , previousCursorKeys :: [MetaKey]

    , addNextParamKeys :: [MetaKey]
    , paramOrderBeforeKeys :: [MetaKey]
    , paramOrderAfterKeys :: [MetaKey]
    , jumpToDefinitionKeys :: [MetaKey]
    , delForwardKeys :: [MetaKey]
    , delBackwardKeys :: [MetaKey]
    , actionKeys :: [MetaKey]
    , replaceParentKeys :: [MetaKey]
    , attachKeys :: [MetaKey]
    , detachKeys :: [MetaKey]
    , parenDetachKeys :: [MetaKey]

    , letAddItemKeys :: [MetaKey]

    , extractKeys :: [MetaKey]
    , inlineKeys :: [MetaKey]
    , moveLetInwardKeys:: [MetaKey]

    , enterSubexpressionKeys :: [MetaKey]
    , leaveSubexpressionKeys :: [MetaKey]

    , recordOpenKeys :: [MetaKey]
    , recordCloseKeys :: [MetaKey]
    , recordAddFieldKeys :: [MetaKey]

    , caseOpenKeys :: [MetaKey]
    , caseAddAltKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Config

Lens.makeLenses ''Config

class HasConfig env where config :: Lens' env Config
instance HasConfig Config where config = id

delKeys :: (MonadReader env m, HasConfig env) => m [MetaKey]
delKeys = sequence [Lens.view config <&> delForwardKeys, Lens.view config <&> delBackwardKeys] <&> mconcat
