{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetsEnvT
    ( WidgetEnvT, runWidgetEnvT
    , mapWidgetEnvT

    , readCursor, subCursor, isSubCursor

    , Env(..), envCursor, envTextStyle

    , readEnv

    , localEnv
    , envAssignCursor, envAssignCursorPrefix

    , readTextStyle
    , setTextColor
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Data.Maybe (isJust)
import           Data.Vector.Vector2 (Vector2)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

import           Prelude.Compat

data Env = Env
    { _envCursor :: Widget.Id
    , _envTextStyle :: TextEdit.Style
    , stdSpacing :: Vector2 Double
    }
Lens.makeLenses ''Env

newtype WidgetEnvT m a = WidgetEnvT
    { _widgetEnvT :: ReaderT Env m a
    } deriving (Functor, Applicative, Monad, MonadTrans)
Lens.makeLenses ''WidgetEnvT

runWidgetEnvT :: Env -> WidgetEnvT m a -> m a
runWidgetEnvT env (WidgetEnvT action) = runReaderT action env

mapWidgetEnvT :: (m a -> n a) -> WidgetEnvT m a -> WidgetEnvT n a
mapWidgetEnvT = (widgetEnvT %~) . Reader.mapReaderT

readEnv :: Monad m => WidgetEnvT m Env
readEnv = WidgetEnvT Reader.ask

readCursor :: Monad m => WidgetEnvT m Widget.Id
readCursor = readEnv <&> (^. envCursor)

subCursor :: Monad m => Widget.Id -> WidgetEnvT m (Maybe AnimId)
subCursor folder = readCursor <&> Widget.subId folder

isSubCursor :: Monad m => Widget.Id -> WidgetEnvT m Bool
isSubCursor = fmap isJust . subCursor

readTextStyle :: Monad m => WidgetEnvT m TextEdit.Style
readTextStyle = readEnv <&> (^. envTextStyle)

envAssignCursor
    :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursor src dest =
    envCursor %~ replace
    where
        replace cursor
            | cursor == src = dest
            | otherwise = cursor

envAssignCursorPrefix
    :: Widget.Id -> (AnimId -> Widget.Id) -> Env -> Env
envAssignCursorPrefix srcFolder dest =
    envCursor %~ replace
    where
        replace cursor =
            case Widget.subId srcFolder cursor of
            Nothing -> cursor
            Just suffix -> dest suffix

localEnv :: (Env -> Env) -> WidgetEnvT m a -> WidgetEnvT m a
localEnv = (widgetEnvT %~) . Reader.local

setTextColor :: Draw.Color -> Env -> Env
setTextColor color =
    envTextStyle . TextEdit.sTextViewStyle . TextView.styleColor .~ color
