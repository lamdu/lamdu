{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Data(
    Data, boxCursors, textEditModel, isExpanded,
    boxCursor,
    Tree(..), nodeValue, nodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValue, makeNode, makeNodeRef, makeLeafRef)
where

import           Prelude                         hiding ((.), id)
import           Control.Category                ((.))
import           Data.Binary                     (Binary(..))
import           Data.Binary.Utils               (get3, put3)
import           Data.Store.IRef                 (IRef)
import           Data.Store.IRef.Tree            (Tree(..), nodeValue, nodeChildrenRefs)
import           Data.Store.Transaction          (Transaction)
import qualified Data.Store.Transaction          as Transaction
import           Data.Record.Label               ((:->), mkLabels, lens)
import qualified Data.Record.Label.Map           as Label.Map
import qualified Data.Record.Label.Maybe         as Label.Maybe
import qualified Graphics.UI.Bottle.Widgets.Box      as Box
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import           Data.Map                        (Map)
import qualified Data.Map                        as Map

data Data = Data {
  _boxCursors :: Map String Box.Cursor,
  _textEditModel :: TextEdit.Model,
  _isExpanded :: Bool
  }
  deriving (Show, Read, Eq, Ord)
$(mkLabels [''Data])

boxCursor :: Box.Cursor -> String -> Data :-> Box.Cursor
boxCursor d key = Label.Maybe.fromMaybe d . Label.Map.value key . boxCursors

instance Binary Data where
  get = get3 Data
  put (Data a b c) = put3 a b c

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValue :: String -> Data
makeValue text =
  Data {
    _boxCursors = Map.empty,
    _textEditModel = TextEdit.makeModel text,
    _isExpanded = True
  }

makeNode :: String -> [ITreeD] -> TreeD
makeNode = Node . makeValue

makeNodeRef :: Monad m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = Transaction.newIRef $ makeNode text childrenRefs

makeLeafRef :: Monad m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
