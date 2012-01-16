{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Data(
    Data, textEditModel, isExpanded,
    Tree(..), nodeValue, nodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValue, makeNode, makeNodeRef, makeLeafRef)
where

import           Prelude                         hiding ((.), id)
import           Control.Category                ((.))
import           Data.Binary                     (Binary(..))
import           Data.Store.IRef                 (IRef)
import           Data.Store.IRef.Tree            (Tree(..), nodeValue, nodeChildrenRefs)
import           Data.Store.Transaction          (Transaction)
import qualified Data.Store.Transaction          as Transaction
import           Data.Record.Label               ((:->), mkLabels, lens)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)

data Data = Data {
  _textEditModel :: String,
  _isExpanded :: Bool
  }
  deriving (Show, Read, Eq, Ord)
$(mkLabels [''Data])
$(derive makeBinary ''Data)

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValue :: String -> Data
makeValue text =
  Data {
    _textEditModel = text,
    _isExpanded = True
  }

makeNode :: String -> [ITreeD] -> TreeD
makeNode = Node . makeValue

makeNodeRef :: Monad m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = Transaction.newIRef $ makeNode text childrenRefs

makeLeafRef :: Monad m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
