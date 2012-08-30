{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.OTransaction
  ( OTransaction, runOTransaction
  , unWrapInner
  , TWidget, WidgetT
  , readCursor, subCursor, atCursor, assignCursor, assignCursorPrefix
  , readTextStyle, transaction
  , atTextStyle, setTextSizeColor
  , markVariablesAsUsed, usedVariables
  , getP
  , getName
  ) where

{- Outer transaction -}

import Control.Applicative (Applicative, liftA2)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Data.Map (Map)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (MkProperty)
import Editor.ITransaction (ITransaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
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

data NameGenState = NameGenState
  { ngUnusedNames :: [String]
  , ngUsedNames :: Map Guid String
  }

initialNameGenState :: NameGenState
initialNameGenState =
  NameGenState names Map.empty
  where
    alphabet = map (:[]) ['a'..'z']
    names = alphabet ++ liftA2 (++) names alphabet

newtype OTransaction t m a = OTransaction
  { unOTransaction :: RWST OTransactionEnv [Guid] NameGenState (Transaction t m) a
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
  liftM f $ runRWST action (OTransactionEnv cursor style) initialNameGenState
  where
    f (x, _, _) = x

generateNewName :: Monad m => Guid -> OTransaction t m String
generateNewName guid = do
  nameGen <- OTransaction RWS.get
  let (name : nextNames) = ngUnusedNames nameGen
  OTransaction $ RWS.put nameGen
    { ngUnusedNames = nextNames
    , ngUsedNames = Map.insert guid name $ ngUsedNames nameGen
    }
  return name

getName :: Monad m => Guid -> OTransaction t m String
getName guid = do
  storedName <- transaction . Anchors.getP $ Anchors.assocNameRef guid
  -- TODO: maybe use Maybe?
  if null storedName
    then
      maybe (generateNewName guid) return =<<
      (OTransaction . RWS.gets) (Map.lookup guid . ngUsedNames)
    else return storedName

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

assignCursorPrefix
  :: Monad m => Widget.Id -> Widget.Id -> OTransaction t m a -> OTransaction t m a
assignCursorPrefix srcFolder dest =
  atCursor replace
  where
    replace cursor =
      case Widget.subId srcFolder cursor of
      Nothing -> cursor
      Just _ -> dest

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
