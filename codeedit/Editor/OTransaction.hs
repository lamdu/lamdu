{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.OTransaction
  ( OTransaction, runOTransaction
  , unWrapInner
  , WidgetT
  , readCursor, subCursor, isSubCursor
  , Env(..)
  , atEnv, atEnvCursor
  , envAssignCursor, envAssignCursorPrefix
  , assignCursor, assignCursorPrefix

  , Settings(..), vsShowInferredTypes
  , readSettings

  , readTextStyle, transaction
  , atEnvTextStyle, setTextSizeColor, setTextColor
  , getP
  ) where

{- Outer transaction -}

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Maybe (isJust)
import Data.Store.Transaction (Transaction)
import Editor.ITransaction (ITransaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Editor.Anchors as Anchors
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

-- TODO: Move show-help here?
newtype Settings = Settings
  { _vsShowInferredTypes :: Bool
  }
LensTH.makeLenses ''Settings

data Env = Env
  { envCursor :: Widget.Id
  , envTextStyle :: TextEdit.Style
  , envSettings :: Settings
  }
AtFieldTH.make ''Env

type WidgetT t m = Widget (ITransaction t m)

newtype OTransaction t m a = OTransaction
  { unOTransaction :: ReaderT Env (Transaction t m) a
  }
  deriving (Functor, Applicative, Monad)
AtFieldTH.make ''OTransaction

transaction :: Monad m => Transaction t m a -> OTransaction t m a
transaction = OTransaction . lift

getP :: Monad m => Anchors.MkProperty t m a -> OTransaction t m a
getP = transaction . Anchors.getP

runOTransaction
  :: Monad m
  => Widget.Id -> TextEdit.Style -> Settings
  -> OTransaction t m a -> Transaction t m a
runOTransaction cursor style settings (OTransaction action) =
  runReaderT action (Env cursor style settings)

unWrapInner
  :: Monad m
  => (Transaction t0 (Transaction t1 m) a -> Transaction t1 m a)
  -> OTransaction t0 (Transaction t1 m) a
  -> OTransaction t1 m a
unWrapInner unwrap (OTransaction act) =
  transaction . unwrap . runReaderT act =<< OTransaction Reader.ask

readCursor :: Monad m => OTransaction t m Widget.Id
readCursor = OTransaction $ Reader.asks envCursor

readSettings :: Monad m => OTransaction t m Settings
readSettings = OTransaction $ Reader.asks envSettings

subCursor :: Monad m => Widget.Id -> OTransaction t m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

isSubCursor :: Monad m => Widget.Id -> OTransaction t m Bool
isSubCursor = liftM isJust . subCursor

readTextStyle :: Monad m => OTransaction t m TextEdit.Style
readTextStyle = OTransaction $ Reader.asks envTextStyle

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

assignCursor :: Monad m => Widget.Id -> Widget.Id -> OTransaction t m a -> OTransaction t m a
assignCursor x y = atEnv $ envAssignCursor x y

assignCursorPrefix :: Monad m => Widget.Id -> Widget.Id -> OTransaction t m a -> OTransaction t m a
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

atEnv :: Monad m => (Env -> Env) -> OTransaction t m a -> OTransaction t m a
atEnv = atOTransaction . Reader.local

setTextColor :: Draw.Color -> Env -> Env
setTextColor = atEnvTextStyle . TextEdit.atSTextViewStyle . TextView.atStyleColor . const
