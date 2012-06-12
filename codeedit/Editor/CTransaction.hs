{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.CTransaction(
  CTransaction, runCTransaction, runNestedCTransaction, TWidget, WidgetT,
  readCursor, subCursor, readTextStyle, transaction, getP, atCursor, assignCursor,
  atTextStyle, atTextSizeColor, markVariablesAsUsed, usedVariables)
where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Data.Store.Property(Property)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data CTransactionEnv = CTransactionEnv {
  envCursor :: Widget.Id,
  envTextStyle :: TextEdit.Style
  }
AtFieldTH.make ''CTransactionEnv

type WidgetT t m = Widget (Transaction t m)
type TWidget t m = CTransaction t m (WidgetT t m)

newtype CTransaction t m a = CTransaction {
  unCTransaction :: Reader.ReaderT CTransactionEnv (Writer.WriterT [Data.VariableRef] (Transaction t m)) a
  }
  deriving (Monad)
AtFieldTH.make ''CTransaction

liftEnv
  :: Reader.ReaderT CTransactionEnv (Writer.WriterT [Data.VariableRef] (Transaction t m)) a
  -> CTransaction t m a
liftEnv = CTransaction

liftUsedvars :: Monad m => Writer.WriterT [Data.VariableRef] (Transaction t m) a -> CTransaction t m a
liftUsedvars = liftEnv . lift

transaction :: Monad m => Transaction t m a -> CTransaction t m a
transaction = liftUsedvars . lift

runCTransaction
  :: Monad m
  => Widget.Id -> TextEdit.Style
  -> CTransaction t m a -> Transaction t m a
runCTransaction cursor style =
  liftM fst . Writer.runWriterT .
  (`Reader.runReaderT` CTransactionEnv cursor style) .
  unCTransaction

markVariablesAsUsed :: Monad m => [Data.VariableRef] -> CTransaction t m ()
markVariablesAsUsed = liftUsedvars . Writer.tell

usedVariables :: Monad m => CTransaction t m a -> CTransaction t m (a, [Data.VariableRef])
usedVariables = atCTransaction $ Reader.mapReaderT Writer.listen

runNestedCTransaction ::
  Monad m => Transaction.Store t0 (Transaction t1 m) ->
  CTransaction t0 (Transaction t1 m) a ->
  CTransaction t1 m a
runNestedCTransaction store act = do
  cursor <- readCursor
  style <- readTextStyle
  transaction . Transaction.run store $
    runCTransaction cursor style act

readCursor :: Monad m => CTransaction t m Widget.Id
readCursor = liftEnv $ Reader.asks envCursor

subCursor :: Monad m => Widget.Id -> CTransaction t m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

readTextStyle :: Monad m => CTransaction t m TextEdit.Style
readTextStyle = liftEnv $ Reader.asks envTextStyle

getP :: Monad m => Property (Transaction t m) a -> CTransaction t m a
getP = transaction . Property.get

atCursor :: (Widget.Id -> Widget.Id) -> CTransaction t m a -> CTransaction t m a
atCursor = atCTransaction . Reader.withReaderT . atEnvCursor

assignCursor :: Widget.Id -> Widget.Id -> CTransaction t m a -> CTransaction t m a
assignCursor src dest =
  atCursor replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

atTextStyle
  :: (TextEdit.Style -> TextEdit.Style)
  -> CTransaction t m (Widget f) -> CTransaction t m (Widget f)
atTextStyle = atCTransaction . Reader.withReaderT . atEnvTextStyle

-- Todo: set, not at
atTextSizeColor
  :: Int
  -> Draw.Color
  -> CTransaction t m (Widget f)
  -> CTransaction t m (Widget f)
atTextSizeColor textSize textColor =
  (atTextStyle . TextEdit.atSTextViewStyle)
  ((TextView.atStyleFontSize . const) textSize .
   (TextView.atStyleColor . const) textColor)
