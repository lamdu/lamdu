{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.GUI.WidgetEnvT
  ( WidgetEnvT, runWidgetEnvT
  , mapWidgetEnvT
  , readCursor, subCursor, isSubCursor

  , Env(..), envCursor, envTextStyle

  , localEnv
  , envAssignCursor, envAssignCursorPrefix
  , assignCursor, assignCursorPrefix

  , readConfig, readTextStyle
  , setTextSizeColor, setTextColor
  , getP
  ) where

import Control.Applicative (Applicative)
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Maybe (isJust)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Lamdu.Config (Config)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data Env = Env
  { _envCursor :: Widget.Id
  , _envTextStyle :: TextEdit.Style
  , _envConfig :: Config
  }
Lens.makeLenses ''Env

newtype WidgetEnvT m a = WidgetEnvT
  { _widgetEnvT :: ReaderT Env m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)
Lens.makeLenses ''WidgetEnvT

-- TODO: Remove this
getP :: MonadA m => Transaction.MkProperty m a -> WidgetEnvT (Transaction m) a
getP = lift . Transaction.getP

runWidgetEnvT :: MonadA m => Widget.Id -> TextEdit.Style -> Config -> WidgetEnvT m a -> m a
runWidgetEnvT cursor style config (WidgetEnvT action) = runReaderT action (Env cursor style config)

mapWidgetEnvT
  :: MonadA m
  => (m a -> n a)
  -> WidgetEnvT m a
  -> WidgetEnvT n a
mapWidgetEnvT = (widgetEnvT %~) . Reader.mapReaderT

readCursor :: MonadA m => WidgetEnvT m Widget.Id
readCursor = WidgetEnvT $ Reader.asks (^. envCursor)

subCursor :: MonadA m => Widget.Id -> WidgetEnvT m (Maybe AnimId)
subCursor folder = fmap (Widget.subId folder) readCursor

isSubCursor :: MonadA m => Widget.Id -> WidgetEnvT m Bool
isSubCursor = fmap isJust . subCursor

readConfig :: MonadA m => WidgetEnvT m Config
readConfig = WidgetEnvT $ Lens.view envConfig

readTextStyle :: MonadA m => WidgetEnvT m TextEdit.Style
readTextStyle = WidgetEnvT $ Lens.view envTextStyle

envAssignCursor
  :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursor src dest =
  envCursor %~ replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

envAssignCursorPrefix
  :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursorPrefix srcFolder dest =
  envCursor %~ replace
  where
    replace cursor =
      case Widget.subId srcFolder cursor of
      Nothing -> cursor
      Just _ -> dest

assignCursor :: MonadA m => Widget.Id -> Widget.Id -> WidgetEnvT m a -> WidgetEnvT m a
assignCursor x y = localEnv $ envAssignCursor x y

assignCursorPrefix :: MonadA m => Widget.Id -> Widget.Id -> WidgetEnvT m a -> WidgetEnvT m a
assignCursorPrefix x y = localEnv $ envAssignCursorPrefix x y

setTextSizeColor
  :: Int
  -> Draw.Color
  -> Env
  -> Env
setTextSizeColor textSize textColor env =
  env
  & envTextStyle . TextEdit.sTextViewStyle %~
    (TextView.styleFontSize .~ textSize) .
    (TextView.styleColor .~ textColor)

localEnv :: MonadA m => (Env -> Env) -> WidgetEnvT m a -> WidgetEnvT m a
localEnv = (widgetEnvT %~) . Reader.local

setTextColor :: Draw.Color -> Env -> Env
setTextColor color =
  envTextStyle . TextEdit.sTextViewStyle . TextView.styleColor .~ color
