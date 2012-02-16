{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Editor.CTransaction(
  CTransaction, runCTransaction, runNestedCTransaction, TWidget,
  readCursor, subCursor, readTextStyle, transaction, getP, assignCursor, atTextStyle)
where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Data.Store.Property(Property)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

data CTransactionEnv = CTransactionEnv {
  envCursor :: Widget.Id,
  envTextStyle :: TextEdit.Style
  }
AtFieldTH.make ''CTransactionEnv

newtype CTransaction t m a = CTransaction {
  unCTransaction :: Reader.ReaderT CTransactionEnv (Transaction t m) a
  }
  deriving (Monad)
AtFieldTH.make ''CTransaction

type TWidget t m = CTransaction t m (Widget (Transaction t m))

runCTransaction :: Widget.Id -> TextEdit.Style -> CTransaction t m a -> Transaction t m a
runCTransaction cursor style =
  (`Reader.runReaderT` CTransactionEnv cursor style) . unCTransaction

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
readCursor = CTransaction (Reader.asks envCursor)

subCursor :: Monad m => Widget.Id -> CTransaction t m (Maybe AnimId)
subCursor folder = liftM (Widget.subId folder) readCursor

readTextStyle :: Monad m => CTransaction t m TextEdit.Style
readTextStyle = CTransaction (Reader.asks envTextStyle)

transaction :: Monad m => Transaction t m a -> CTransaction t m a
transaction = CTransaction . lift

getP :: Monad m => Property (Transaction t m) a -> CTransaction t m a
getP = transaction . Property.get

assignCursor :: Widget.Id -> Widget.Id -> CTransaction t m a -> CTransaction t m a
assignCursor src dest =
  (atCTransaction . Reader.withReaderT . atEnvCursor) replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

atTextStyle :: (TextEdit.Style -> TextEdit.Style) -> TWidget t m -> TWidget t m
atTextStyle = atCTransaction . Reader.withReaderT . atEnvTextStyle
