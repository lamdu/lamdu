{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.OTransaction
  ( OTransaction, runOTransaction
  , unWrapInner
  , TWidget, WidgetT
  , readCursor, subCursor, atCursor, assignCursor
  , readTextStyle, transaction
  , atTextStyle, setTextSizeColor
  , markVariablesAsUsed, usedVariables
  , getP
  ) where

{- Outer transaction -}

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (MkProperty)
import Editor.ITransaction (ITransaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Control.Monad.Trans.RWS (RWST, runRWST)
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data OTransactionEnv = OTransactionEnv {
  envCursor :: Widget.Id,
  envTextStyle :: TextEdit.Style
  }
AtFieldTH.make ''OTransactionEnv

type WidgetT t m = Widget (ITransaction t m)
type TWidget t m = OTransaction t m (WidgetT t m)

newtype OTransaction t m a = OTransaction
  { unOTransaction :: RWST OTransactionEnv [Guid] () (Transaction t m) a
  }
  deriving (Functor, Applicative, Monad)
AtFieldTH.make ''OTransaction

transaction :: Monad m => Transaction t m a -> OTransaction t m a
transaction = OTransaction . lift

runOTransaction
  :: Monad m
  => Widget.Id -> TextEdit.Style
  -> OTransaction t m a -> Transaction t m a
runOTransaction cursor style (OTransaction action) =
  liftM f $ runRWST action (OTransactionEnv cursor style) ()
  where
    f (x, _, _) = x

markVariablesAsUsed :: Monad m => [Guid] -> OTransaction t m ()
markVariablesAsUsed = OTransaction . RWS.tell

usedVariables
  :: Monad m
  => OTransaction t m a -> OTransaction t m (a, [Guid])
usedVariables = atOTransaction RWS.listen

unWrapInner
  :: Monad m
  => (Transaction t0 (Transaction t1 m) a -> Transaction t1 m a)
  -> OTransaction t0 (Transaction t1 m) a
  -> OTransaction t1 m a
unWrapInner unwrap act = do
  cursor <- readCursor
  style <- readTextStyle
  transaction . unwrap $ runOTransaction cursor style act

readCursor :: Monad m => OTransaction t m Widget.Id
readCursor = OTransaction $ RWS.asks envCursor

subCursor :: Monad m => Widget.Id -> OTransaction t m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

readTextStyle :: Monad m => OTransaction t m TextEdit.Style
readTextStyle = OTransaction $ RWS.asks envTextStyle

atCursor
  :: Monad m => (Widget.Id -> Widget.Id) -> OTransaction t m a -> OTransaction t m a
atCursor = atOTransaction . RWS.local . atEnvCursor

assignCursor
  :: Monad m => Widget.Id -> Widget.Id -> OTransaction t m a -> OTransaction t m a
assignCursor src dest =
  atCursor replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

atTextStyle
  :: Monad m
  => (TextEdit.Style -> TextEdit.Style)
  -> OTransaction t m a -> OTransaction t m a
atTextStyle = atOTransaction . RWS.local . atEnvTextStyle

setTextSizeColor
  :: Monad m
  => Int
  -> Draw.Color
  -> OTransaction t m (Widget f)
  -> OTransaction t m (Widget f)
setTextSizeColor textSize textColor =
  (atTextStyle . TextEdit.atSTextViewStyle)
  ((TextView.atStyleFontSize . const) textSize .
   (TextView.atStyleColor . const) textColor)

getP :: Monad m => MkProperty t m a -> OTransaction t m a
getP = transaction . liftM Property.value
