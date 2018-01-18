{-# OPTIONS -O0 #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.Config
    ( Export(..), Pane(..), Completion(..)
    , Debug(..)
    , Eval(..)
    , Literal(..)
    , Config(..)
    , HasConfig(..)
    , delKeys
    , configMenu
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
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
    , importKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Export

data Pane = Pane
    { paneCloseKeys :: [MetaKey]
    , paneMoveDownKeys :: [MetaKey]
    , paneMoveUpKeys :: [MetaKey]
    , newDefinitionKeys :: [MetaKey]
    , newDefinitionButtonPressKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Pane

data Completion = Completion
    { completionJumpToNextKeys :: [MetaKey]
    , completionJumpToPrevKeys :: [MetaKey]
    , completionResultCount :: Int
    , completionOpenKeys :: [MetaKey]
    , completionCloseKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Completion

data Eval = Eval
    { prevScopeKeys :: [MetaKey]
    , nextScopeKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Eval

data Literal = Literal
    { literalStartEditingKeys :: [MetaKey]
    , literalStopEditingKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Literal

data Debug = Debug
    { debugShowFPS :: Bool
    , virtualCursorShown :: Bool
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Debug

data Config = Config
    { zoom :: Zoom.Config
    , export :: Export
    , pane :: Pane
    , versionControl :: VersionControl.Config
    , completion :: Completion
    , literal :: Literal
    , eval :: Eval
    , debug :: Debug
    , menu :: Menu.Keys

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
deriveJSON defaultOptions ''Config

class HasConfig env where config :: Lens' env Config
instance HasConfig Config where config = id

delKeys :: (MonadReader env m, HasConfig env) => m [MetaKey]
delKeys = sequence [Lens.view config <&> delForwardKeys, Lens.view config <&> delBackwardKeys] <&> mconcat

configMenu :: Lens' Config Menu.Keys
configMenu f c = menu c & f <&> \menu' -> c { menu = menu' }
