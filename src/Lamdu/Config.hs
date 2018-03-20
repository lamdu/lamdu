{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
module Lamdu.Config where

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
    { _exportPath :: FilePath
    , _exportKeys :: [MetaKey]
    , _exportFancyKeys :: [MetaKey]
    , _exportAllKeys :: [MetaKey]
    , _executeKeys :: [MetaKey]
    , _importKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Export

Lens.makeLenses ''Export

data Pane = Pane
    { _paneCloseKeys :: [MetaKey]
    , _paneMoveDownKeys :: [MetaKey]
    , _paneMoveUpKeys :: [MetaKey]
    , _newDefinitionKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
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
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Completion

Lens.makeLenses ''Completion

data Eval = Eval
    { _prevScopeKeys :: [MetaKey]
    , _nextScopeKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Eval

Lens.makeLenses ''Eval

data Literal = Literal
    { _literalStartEditingKeys :: [MetaKey]
    , _literalStopEditingKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Literal

Lens.makeLenses ''Literal

data Debug = Debug
    { _debugShowFPS :: Bool
    , _virtualCursorShown :: Bool
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Debug

Lens.makeLenses ''Debug

data Config = Config
    { _zoom :: Zoom.Config
    , _export :: Export
    , _pane :: Pane
    , _versionControl :: VersionControl.Config
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
    , _attachKeys :: [MetaKey]
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
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removeOptionalUnderscore}
#endif
    ''Config

Lens.makeLenses ''Config

class HasConfig env where config :: Lens' env Config
instance HasConfig Config where config = id

delKeys :: (MonadReader env m, HasConfig env) => m [MetaKey]
delKeys = sequence [Lens.view (config . delForwardKeys), Lens.view (config . delBackwardKeys)] <&> mconcat
