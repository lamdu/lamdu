{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Editor.Data(
    Data(..), atIsExpanded, atTextEditModel,
    Tree(..), atNodeValue, atNodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValue, makeNode, makeNodeRef, makeLeafRef)
where

import Control.Category ((.))
import Data.Binary (Binary(..))
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.IRef (IRef)
import Data.Store.IRef.Tree (Tree(..), atNodeValue, atNodeChildrenRefs)
import Data.Store.Transaction (Transaction)
import Prelude hiding ((.), id)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Transaction as Transaction

data Data = Data {
  textEditModel :: String,
  isExpanded :: Bool
  }
  deriving (Show, Read, Eq, Ord)
AtFieldTH.make ''Data
derive makeBinary ''Data

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValue :: String -> Data
makeValue text =
  Data {
    textEditModel = text,
    isExpanded = True
  }

makeNode :: String -> [ITreeD] -> TreeD
makeNode = Node . makeValue

makeNodeRef :: Monad m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = Transaction.newIRef $ makeNode text childrenRefs

makeLeafRef :: Monad m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
