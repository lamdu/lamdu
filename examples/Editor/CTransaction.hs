{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Editor.CTransaction(
  CTransaction, runCTransaction, TWidget,
  readCursor, readTextStyle, transaction, getP, assignCursor, atEmptyStr)
where

import Control.Monad.Trans.Class (lift)
import Data.Store.Property(Property)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

data CTransactionEnv = CTransactionEnv {
  envCursor :: Widget.Cursor,
  envTextStyle :: TextEdit.Style
  }
AtFieldTH.make ''CTransactionEnv

newtype CTransaction t m a = CTransaction {
  unCTransaction :: Reader.ReaderT CTransactionEnv (Transaction t m) a
  }
  deriving (Monad)
AtFieldTH.make ''CTransaction

type TWidget t m = CTransaction t m (Widget (Transaction t m))

runCTransaction :: Widget.Cursor -> TextEdit.Style -> CTransaction t m a -> Transaction t m a
runCTransaction cursor style =
  (`Reader.runReaderT` CTransactionEnv cursor style) . unCTransaction

readCursor :: Monad m => CTransaction t m Widget.Cursor
readCursor = CTransaction (Reader.asks envCursor)

readTextStyle :: Monad m => CTransaction t m TextEdit.Style
readTextStyle = CTransaction (Reader.asks envTextStyle)

transaction :: Monad m => Transaction t m a -> CTransaction t m a
transaction = CTransaction . lift

getP :: Monad m => Property (Transaction t m) a -> CTransaction t m a
getP = transaction . Property.get

assignCursor :: Widget.Cursor -> Widget.Cursor -> CTransaction t m a -> CTransaction t m a
assignCursor src dest =
  (atCTransaction . Reader.withReaderT . atEnvCursor) replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

atEmptyStr :: (String -> String) -> TWidget t m -> TWidget t m
atEmptyStr = atCTransaction . Reader.withReaderT . atEnvTextStyle . TextEdit.atSEmptyString
