{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.WidgetEnvT
  ( WidgetEnvT, runWidgetEnvT
  , mapWidgetEnvT
  , readCursor, subCursor, isSubCursor

  , Env(..), envCursor, envTextStyle

  , atEnv
  , envAssignCursor, envAssignCursorPrefix
  , assignCursor, assignCursorPrefix

  , readTextStyle
  , setTextSizeColor, setTextColor
  , getP
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Maybe (isJust)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Editor.Anchors as Anchors
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data Env = Env
  { _envCursor :: Widget.Id
  , _envTextStyle :: TextEdit.Style
  }
LensTH.makeLenses ''Env

newtype WidgetEnvT m a = WidgetEnvT
  { _widgetEnvT :: ReaderT Env m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)
LensTH.makeLenses ''WidgetEnvT

-- TODO: Remove this
getP :: Monad m => Anchors.MkProperty t m a -> WidgetEnvT (Transaction t m) a
getP = lift . Anchors.getP

runWidgetEnvT :: Monad m => Widget.Id -> TextEdit.Style -> WidgetEnvT m a -> m a
runWidgetEnvT cursor style (WidgetEnvT action) = runReaderT action (Env cursor style)

mapWidgetEnvT
  :: Monad m
  => (m a -> n a)
  -> WidgetEnvT m a
  -> WidgetEnvT n a
mapWidgetEnvT = Lens.over widgetEnvT . Reader.mapReaderT

readCursor :: Monad m => WidgetEnvT m Widget.Id
readCursor = WidgetEnvT . Reader.asks $ Lens.view envCursor

subCursor :: Monad m => Widget.Id -> WidgetEnvT m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

isSubCursor :: Monad m => Widget.Id -> WidgetEnvT m Bool
isSubCursor = liftM isJust . subCursor

readTextStyle :: Monad m => WidgetEnvT m TextEdit.Style
readTextStyle = WidgetEnvT . Reader.asks $ Lens.view envTextStyle

envAssignCursor
  :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursor src dest =
  Lens.over envCursor replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

envAssignCursorPrefix
  :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursorPrefix srcFolder dest =
  Lens.over envCursor replace
  where
    replace cursor =
      case Widget.subId srcFolder cursor of
      Nothing -> cursor
      Just _ -> dest

assignCursor :: Monad m => Widget.Id -> Widget.Id -> WidgetEnvT m a -> WidgetEnvT m a
assignCursor x y = atEnv $ envAssignCursor x y

assignCursorPrefix :: Monad m => Widget.Id -> Widget.Id -> WidgetEnvT m a -> WidgetEnvT m a
assignCursorPrefix x y = atEnv $ envAssignCursorPrefix x y

setTextSizeColor
  :: Int
  -> Draw.Color
  -> Env
  -> Env
setTextSizeColor textSize textColor =
  Lens.over (envTextStyle . TextEdit.sTextViewStyle)
  (Lens.set TextView.styleFontSize textSize .
   Lens.set TextView.styleColor textColor)

atEnv :: Monad m => (Env -> Env) -> WidgetEnvT m a -> WidgetEnvT m a
atEnv = Lens.over widgetEnvT . Reader.local

setTextColor :: Draw.Color -> Env -> Env
setTextColor = Lens.set (envTextStyle . TextEdit.sTextViewStyle . TextView.styleColor)
