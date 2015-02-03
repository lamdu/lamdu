{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetsEnvT
  ( WidgetEnvT, runWidgetEnvT
  , mapWidgetEnvT

  , readCursor, subCursor, isSubCursor

  , Env(..), envCursor, envTextStyle

  , readEnv

  , localEnv
  , envAssignCursor, envAssignCursorPrefix
  , assignCursor, assignCursorPrefix

  , readTextStyle
  , setTextSizeColor, setTextColor
  ) where

import           Control.Applicative (Applicative)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.MonadA (MonadA)
import           Data.Maybe (isJust)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizedFont as SizedFont
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data Env = Env
  { _envCursor :: Widget.Id
  , _envTextStyle :: TextEdit.Style
  , backgroundCursorId :: AnimId
  , cursorBGColor :: Draw.Color
  , layerCursor :: Anim.Layer
  , layerInterval :: Anim.Layer
  , verticalSpacing :: Double
  , stdSpaceWidth :: Double
  }
Lens.makeLenses ''Env

newtype WidgetEnvT m a = WidgetEnvT
  { _widgetEnvT :: ReaderT Env m a
  } deriving (Functor, Applicative, Monad, MonadTrans)
Lens.makeLenses ''WidgetEnvT

runWidgetEnvT ::
  MonadA m => Env -> WidgetEnvT m a -> m a
runWidgetEnvT env (WidgetEnvT action) = runReaderT action env

mapWidgetEnvT
  :: MonadA m
  => (m a -> n a)
  -> WidgetEnvT m a
  -> WidgetEnvT n a
mapWidgetEnvT = (widgetEnvT %~) . Reader.mapReaderT

readEnv :: MonadA m => WidgetEnvT m Env
readEnv = WidgetEnvT Reader.ask

readCursor :: MonadA m => WidgetEnvT m Widget.Id
readCursor = readEnv <&> (^. envCursor)

subCursor :: MonadA m => Widget.Id -> WidgetEnvT m (Maybe AnimId)
subCursor folder = readCursor <&> Widget.subId folder

isSubCursor :: MonadA m => Widget.Id -> WidgetEnvT m Bool
isSubCursor = fmap isJust . subCursor

readTextStyle :: MonadA m => WidgetEnvT m TextEdit.Style
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

assignCursor ::
  MonadA m => Widget.Id -> Widget.Id ->
  WidgetEnvT m a -> WidgetEnvT m a
assignCursor x y = localEnv $ envAssignCursor x y

assignCursorPrefix ::
  MonadA m => Widget.Id -> (AnimId -> Widget.Id) ->
  WidgetEnvT m a -> WidgetEnvT m a
assignCursorPrefix x y = localEnv $ envAssignCursorPrefix x y

setTextSizeColor :: Double -> Draw.Color -> Env -> Env
setTextSizeColor textSize textColor env =
  env
  & envTextStyle . TextEdit.sTextViewStyle %~
    (TextView.styleFont . SizedFont.fontSize .~ textSize) .
    (TextView.styleColor .~ textColor)

localEnv :: MonadA m => (Env -> Env) -> WidgetEnvT m a -> WidgetEnvT m a
localEnv = (widgetEnvT %~) . Reader.local

setTextColor :: Draw.Color -> Env -> Env
setTextColor color =
  envTextStyle . TextEdit.sTextViewStyle . TextView.styleColor .~ color
