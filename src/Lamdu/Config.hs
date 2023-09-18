{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Lamdu.Config where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import           GUI.Momentu (ModKey)
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Zoom as Zoom
import qualified Lamdu.Debug.Tasks as Debug
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import           Lamdu.Prelude

data Export key = Export
    { _exportPath :: FilePath
    , _exportKeys :: [key]
    , _exportFancyKeys :: [key]
    , _exportAllKeys :: [key]
    , _executeKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_" ''Export

Lens.makeLenses ''Export

data Pane key = Pane
    { _paneCloseKeys :: [key]
    , _paneMoveDownKeys :: [key]
    , _paneMoveUpKeys :: [key]
    , _newDefinitionKeys :: [key]
    , _nextOutdatedKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^. Lens.failing (prefixed "pane") id)
        . (^?! prefixed "_")
    }
    ''Pane

Lens.makeLenses ''Pane

newtype Completion key = Completion
    { _completionResultCount :: Int
    } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_completion" ''Completion

Lens.makeLenses ''Completion

data Eval key = Eval
    { _prevScopeKeys :: [key]
    , _nextScopeKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_" ''Eval

Lens.makeLenses ''Eval

data Literal key = Literal
    { _literalStartEditingKeys :: [key]
    , _literalStopEditingKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_literal" ''Literal

Lens.makeLenses ''Literal

data Debug key = Debug
    { _debugShowFPS :: Bool
    , _printCursor :: Bool
    , _printEvents :: Bool
    , _virtualCursorShown :: Bool
    , _breakpoints :: Debug.Tasks Bool
    , _jumpToSourceKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^. Lens.failing (prefixed "debug") id)
        . (^?! prefixed "_")
    }
    ''Debug

Lens.makeLenses ''Debug

data Config key = Config
    { _zoom :: Zoom.Config key
    , _export :: Export key
    , _pane :: Pane key
    , _versionControl :: VersionControl.Config key
    , _completion :: Completion key
    , _literal :: Literal key
    , _eval :: Eval key
    , _debug :: Debug key
    , _searchMenu :: SearchMenu.Config key
    , _grid :: Grid.Keys key
    , _textEdit :: TextEdit.Keys key
    , _dirKeys :: StdKeys.DirKeys key
    , _orderDirKeys :: StdKeys.DirKeys key

    , _maxExprDepth :: Int

    , _helpKeys :: [key]
    , _quitKeys :: [key]
    , _changeThemeKeys :: [key]
    , _changeLanguageKeys :: [key]
    , _nextAnnotationModeKeys :: [key]
    , _previousCursorKeys :: [key]

    , _addNextParamKeys :: [key]
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

    , _enterSubexpressionKeys :: [key]
    , _leaveSubexpressionKeys :: [key]

    , _compositeOpenKeys :: [key]
    , _compositeAddItemKeys :: [key]

    , _injectValueKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_" ''Config

Lens.makeLensesWith (Lens.lensRules & Lens.generateRecordSyntax .~ True) ''Config

hasConfig :: Has (Config ModKey) env => Lens' env (Config ModKey)
hasConfig = has

delKeys :: (MonadReader env m, Has (Config ModKey) env) => m [ModKey]
delKeys =
    Lens.view hasConfig
    <&> \config -> config ^. delForwardKeys <> config ^. delBackwardKeys
