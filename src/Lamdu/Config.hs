{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeApplications #-}
module Lamdu.Config where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Zoom as Zoom
import qualified Lamdu.Debug.Tasks as Debug
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import qualified Lamdu.Sugar.Config as Sugar

import           Lamdu.Prelude

data Export key = Export
    { _exportPath :: FilePath
    , _exportKeys :: [key]
    , _exportFancyKeys :: [key]
    , _exportAllKeys :: [key]
    , _executeKeys :: [key]
    , _importKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_" ''Export

Lens.makeLenses ''Export

data Pane key = Pane
    { _paneCloseKeys :: [key]
    , _paneMoveDownKeys :: [key]
    , _paneMoveUpKeys :: [key]
    , _newDefinitionKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^. Lens.failing (prefixed "pane") id)
        . (^?! prefixed "_")
    }
    ''Pane

Lens.makeLenses ''Pane

data Completion key = Completion
    { _completionJumpToNextKeys :: [key]
    , _completionJumpToPrevKeys :: [key]
    , _completionResultCount :: Int
    } deriving (Eq, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_completion" ''Completion

Lens.makeLenses ''Completion

data Eval key = Eval
    { _prevScopeKeys :: [key]
    , _nextScopeKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_" ''Eval

Lens.makeLenses ''Eval

data Literal key = Literal
    { _literalStartEditingKeys :: [key]
    , _literalStopEditingKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_literal" ''Literal

Lens.makeLenses ''Literal

data Debug key = Debug
    { _debugShowFPS :: Bool
    , _printCursor :: Bool
    , _printEvents :: Bool
    , _virtualCursorShown :: Bool
    , _breakpoints :: Debug.Tasks Bool
    , _jumpToSourceKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^. Lens.failing (prefixed "debug") id)
        . (^?! prefixed "_")
    }
    ''Debug

Lens.makeLenses ''Debug

data Config key = Config
    { _zoom :: Zoom.Config
    , _export :: Export key
    , _pane :: Pane key
    , _versionControl :: VersionControl.Config
    , _sugar :: Sugar.Config
    , _completion :: Completion key
    , _literal :: Literal key
    , _eval :: Eval key
    , _debug :: Debug key
    , _searchMenu :: SearchMenu.Config

    , _maxExprDepth :: Int

    , _helpKeys :: [key]
    , _quitKeys :: [key]
    , _changeThemeKeys :: [key]
    , _changeLanguageKeys :: [key]
    , _nextAnnotationModeKeys :: [key]
    , _previousCursorKeys :: [key]

    , _addNextParamKeys :: [key]
    , _paramOrderBeforeKeys :: [key]
    , _paramOrderAfterKeys :: [key]
    , _jumpToDefinitionKeys :: [key]
    , _delForwardKeys :: [key]
    , _delBackwardKeys :: [key]
    , _actionKeys :: [key]
    , _replaceParentKeys :: [key]
    , _healKeys :: [key]

    , _letAddItemKeys :: [key]

    , _extractKeys :: [key]
    , _inlineKeys :: [key]
    , _moveLetInwardKeys:: [key]
    , _swapWithLeftKeys :: [key]
    , _swapWithRightKeys :: [key]

    , _enterSubexpressionKeys :: [key]
    , _leaveSubexpressionKeys :: [key]

    , _recordOpenKeys :: [key]
    , _recordCloseKeys :: [key]
    , _recordAddFieldKeys :: [key]

    , _caseOpenKeys :: [key]
    , _caseAddAltKeys :: [key]

    , _injectValueKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_" ''Config

Lens.makeLenses ''Config

hasConfig :: Has (Config MetaKey) env => Lens' env (Config MetaKey)
hasConfig = has @(Config MetaKey)

instance Has Sugar.Config (Config key) where has = sugar

delKeys :: (MonadReader env m, Has (Config MetaKey) env) => m [MetaKey]
delKeys =
    Lens.view hasConfig
    <&> \config -> config ^. delForwardKeys <> config ^. delBackwardKeys
