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
import Data.Store.Transaction (Transaction)
import Editor.Anchors (MkProperty)
import Editor.ITransaction (ITransaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Editor.Data as Data
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

newtype OTransaction t m a = OTransaction {
  unOTransaction
  :: Reader.ReaderT OTransactionEnv
     (Writer.WriterT [Data.VariableRef]
      (Transaction t m)) a
  }
  deriving (Functor, Applicative, Monad)
AtFieldTH.make ''OTransaction

liftEnv
  :: Reader.ReaderT OTransactionEnv
     (Writer.WriterT [Data.VariableRef] (Transaction t m)) a
  -> OTransaction t m a
liftEnv = OTransaction

liftUsedvars
  :: Monad m
  => Writer.WriterT [Data.VariableRef] (Transaction t m) a
  -> OTransaction t m a
liftUsedvars = liftEnv . lift

transaction :: Monad m => Transaction t m a -> OTransaction t m a
transaction = liftUsedvars . lift

runOTransaction
  :: Monad m
  => Widget.Id -> TextEdit.Style
  -> OTransaction t m a -> Transaction t m a
runOTransaction cursor style =
  liftM fst . Writer.runWriterT .
  (`Reader.runReaderT` OTransactionEnv cursor style) .
  unOTransaction

markVariablesAsUsed :: Monad m => [Data.VariableRef] -> OTransaction t m ()
markVariablesAsUsed = liftUsedvars . Writer.tell

usedVariables
  :: Monad m
  => OTransaction t m a -> OTransaction t m (a, [Data.VariableRef])
usedVariables = atOTransaction $ Reader.mapReaderT Writer.listen

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
readCursor = liftEnv $ Reader.asks envCursor

subCursor :: Monad m => Widget.Id -> OTransaction t m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

readTextStyle :: Monad m => OTransaction t m TextEdit.Style
readTextStyle = liftEnv $ Reader.asks envTextStyle

atCursor
  :: (Widget.Id -> Widget.Id) -> OTransaction t m a -> OTransaction t m a
atCursor = atOTransaction . Reader.withReaderT . atEnvCursor

assignCursor
  :: Widget.Id -> Widget.Id -> OTransaction t m a -> OTransaction t m a
assignCursor src dest =
  atCursor replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

atTextStyle
  :: (TextEdit.Style -> TextEdit.Style)
  -> OTransaction t m a -> OTransaction t m a
atTextStyle = atOTransaction . Reader.withReaderT . atEnvTextStyle

setTextSizeColor
  :: Int
  -> Draw.Color
  -> OTransaction t m (Widget f)
  -> OTransaction t m (Widget f)
setTextSizeColor textSize textColor =
  (atTextStyle . TextEdit.atSTextViewStyle)
  ((TextView.atStyleFontSize . const) textSize .
   (TextView.atStyleColor . const) textColor)

getP :: Monad m => MkProperty t m a -> OTransaction t m a
getP = transaction . liftM Property.value
