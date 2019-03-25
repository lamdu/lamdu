{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Config where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Zoom as Zoom
import qualified Lamdu.Debug.Tasks as Debug
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import qualified Lamdu.Sugar.Config as Sugar

import           Lamdu.Prelude

data Export = Export
    { _exportPath :: FilePath
    , _exportKeys :: [MetaKey]
    , _exportFancyKeys :: [MetaKey]
    , _exportAllKeys :: [MetaKey]
    , _executeKeys :: [MetaKey]
    , _importKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Export

Lens.makeLenses ''Export

data Pane = Pane
    { _paneCloseKeys :: [MetaKey]
    , _paneMoveDownKeys :: [MetaKey]
    , _paneMoveUpKeys :: [MetaKey]
    , _newDefinitionKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^. Lens.failing (prefixed "pane") id)
        . (^?! prefixed "_")
    }
    ''Pane

Lens.makeLenses ''Pane

data Completion = Completion
    { _completionJumpToNextKeys :: [MetaKey]
    , _completionJumpToPrevKeys :: [MetaKey]
    , _completionResultCount :: Int
    , _completionOpenKeys :: [MetaKey]
    , _completionCloseKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^?! prefixed "_completion")
    }
    ''Completion

Lens.makeLenses ''Completion

data Eval = Eval
    { _prevScopeKeys :: [MetaKey]
    , _nextScopeKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Eval

Lens.makeLenses ''Eval

data Literal = Literal
    { _literalStartEditingKeys :: [MetaKey]
    , _literalStopEditingKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^?! prefixed "_literal")
    }
    ''Literal

Lens.makeLenses ''Literal

data Debug = Debug
    { _debugShowFPS :: Bool
    , _printCursor :: Bool
    , _virtualCursorShown :: Bool
    , _breakpoints :: Debug.Tasks Bool
    , _jumpToSourceKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^. Lens.failing (prefixed "debug") id)
        . (^?! prefixed "_")
    }
    ''Debug

Lens.makeLenses ''Debug

data Config = Config
    { _zoom :: Zoom.Config
    , _export :: Export
    , _pane :: Pane
    , _versionControl :: VersionControl.Config
    , _sugar :: Sugar.Config
    , _completion :: Completion
    , _literal :: Literal
    , _eval :: Eval
    , _debug :: Debug
    , _menu :: Menu.Keys

    , _maxExprDepth :: Int

    , _helpKeys :: [MetaKey]
    , _quitKeys :: [MetaKey]
    , _changeThemeKeys :: [MetaKey]
    , _nextAnnotationModeKeys :: [MetaKey]
    , _previousCursorKeys :: [MetaKey]

    , _addNextParamKeys :: [MetaKey]
    , _paramOrderBeforeKeys :: [MetaKey]
    , _paramOrderAfterKeys :: [MetaKey]
    , _jumpToDefinitionKeys :: [MetaKey]
    , _delForwardKeys :: [MetaKey]
    , _delBackwardKeys :: [MetaKey]
    , _actionKeys :: [MetaKey]
    , _replaceParentKeys :: [MetaKey]
    , _healKeys :: [MetaKey]
    , _detachKeys :: [MetaKey]
    , _parenDetachKeys :: [MetaKey]

    , _letAddItemKeys :: [MetaKey]

    , _extractKeys :: [MetaKey]
    , _inlineKeys :: [MetaKey]
    , _moveLetInwardKeys:: [MetaKey]

    , _enterSubexpressionKeys :: [MetaKey]
    , _leaveSubexpressionKeys :: [MetaKey]

    , _recordOpenKeys :: [MetaKey]
    , _recordCloseKeys :: [MetaKey]
    , _recordAddFieldKeys :: [MetaKey]

    , _caseOpenKeys :: [MetaKey]
    , _caseAddAltKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Config

Lens.makeLenses ''Config

class HasConfig env where config :: Lens' env Config
instance HasConfig Config where config = id

delKeys :: (MonadReader env m, HasConfig env) => m [MetaKey]
delKeys = sequence [Lens.view (config . delForwardKeys), Lens.view (config . delBackwardKeys)] <&> mconcat
