{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.WidgetEnvT
  ( WidgetEnvT, runWidgetEnvT
  , unWrapInner
  , readCursor, subCursor, isSubCursor
  , Env(..)
  , atEnv, atEnvCursor
  , envAssignCursor, envAssignCursorPrefix
  , assignCursor, assignCursorPrefix

  , readTextStyle
  , atEnvTextStyle, setTextSizeColor, setTextColor
  , getP
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Maybe (isJust)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Editor.Anchors as Anchors
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data Env = Env
  { envCursor :: Widget.Id
  , envTextStyle :: TextEdit.Style
  }
AtFieldTH.make ''Env

newtype WidgetEnvT m a = WidgetEnvT
  { unWidgetEnvT :: ReaderT Env m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)
AtFieldTH.make ''WidgetEnvT

-- TODO: Remove this
getP :: Monad m => Anchors.MkProperty t m a -> WidgetEnvT (Transaction t m) a
getP = lift . Anchors.getP

runWidgetEnvT :: Monad m => Widget.Id -> TextEdit.Style -> WidgetEnvT m a -> m a
runWidgetEnvT cursor style (WidgetEnvT action) = runReaderT action (Env cursor style)

unWrapInner
  :: Monad m
  => (m a -> n a)
  -> WidgetEnvT m a
  -> WidgetEnvT n a
unWrapInner = atWidgetEnvT . Reader.mapReaderT

readCursor :: Monad m => WidgetEnvT m Widget.Id
readCursor = WidgetEnvT $ Reader.asks envCursor

subCursor :: Monad m => Widget.Id -> WidgetEnvT m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

isSubCursor :: Monad m => Widget.Id -> WidgetEnvT m Bool
isSubCursor = liftM isJust . subCursor

readTextStyle :: Monad m => WidgetEnvT m TextEdit.Style
readTextStyle = WidgetEnvT $ Reader.asks envTextStyle

envAssignCursor
  :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursor src dest =
  atEnvCursor replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

envAssignCursorPrefix
  :: Widget.Id -> Widget.Id -> Env -> Env
envAssignCursorPrefix srcFolder dest =
  atEnvCursor replace
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
  (atEnvTextStyle . TextEdit.atSTextViewStyle)
  ((TextView.atStyleFontSize . const) textSize .
   (TextView.atStyleColor . const) textColor)

atEnv :: Monad m => (Env -> Env) -> WidgetEnvT m a -> WidgetEnvT m a
atEnv = atWidgetEnvT . Reader.local

setTextColor :: Draw.Color -> Env -> Env
setTextColor = atEnvTextStyle . TextEdit.atSTextViewStyle . TextView.atStyleColor . const
