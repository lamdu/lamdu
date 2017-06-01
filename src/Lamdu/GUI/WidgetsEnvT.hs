{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.GUI.WidgetsEnvT
    ( WidgetEnvT, runWidgetEnvT
    , mapWidgetEnvT

    , Env(..), envCursor, envTextStyle

    , readEnv

    , envAssignCursor, envAssignCursorPrefix

    , readTextStyle
    , textColor
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Vector.Vector2 (Vector2)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.Id as WidgetId
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.GUI.Spacing as Spacing

import           Lamdu.Prelude

data Env = Env
    { _envCursor :: Widget.Id
    , _envTextStyle :: TextEdit.Style
    , _stdSpacing :: Vector2 Double
    }
Lens.makeLenses ''Env

instance Widget.HasCursor Env where cursor = envCursor
instance TextView.HasStyle Env where style = envTextStyle . TextView.style
instance TextEdit.HasStyle Env where style = envTextStyle
instance Spacing.HasStdSpacing Env where stdSpacing = stdSpacing

newtype WidgetEnvT m a = WidgetEnvT
    { _widgetEnvT :: ReaderT Env m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Env)
Lens.makeLenses ''WidgetEnvT

runWidgetEnvT :: Env -> WidgetEnvT m a -> m a
runWidgetEnvT env (WidgetEnvT action) = runReaderT action env

mapWidgetEnvT :: (m a -> n a) -> WidgetEnvT m a -> WidgetEnvT n a
mapWidgetEnvT = (widgetEnvT %~) . Reader.mapReaderT

readEnv :: Monad m => WidgetEnvT m Env
readEnv = WidgetEnvT Reader.ask

readTextStyle :: Monad m => WidgetEnvT m TextEdit.Style
readTextStyle = readEnv <&> (^. envTextStyle)

envAssignCursor :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursor src dest =
    envCursor %~ replace
    where
        replace cursor
            | cursor == src = dest
            | otherwise = cursor

envAssignCursorPrefix :: Widget.Id -> (AnimId -> Widget.Id) -> Env -> Env
envAssignCursorPrefix srcFolder dest =
    envCursor %~ replace
    where
        replace cursor =
            case WidgetId.subId srcFolder cursor of
            Nothing -> cursor
            Just suffix -> dest suffix

localEnv :: Monad m => (Env -> Env) -> WidgetEnvT m a -> WidgetEnvT m a
localEnv = (widgetEnvT %~) . Reader.local

textColor :: Lens' Env Draw.Color
textColor = envTextStyle . TextEdit.sTextViewStyle . TextView.styleColor
