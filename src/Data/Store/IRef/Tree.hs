{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Data.Store.IRef.Tree
    (Tree(..), nodeValue, nodeChildrenRefs)
where

import Data.Binary       (Binary(..))
import Data.Store.IRef   (IRef)
import Data.Record.Label ((:->), mkLabels, lens)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)

data Tree a = Node {
  _nodeValue :: a,
  _nodeChildrenRefs :: [IRef (Tree a)]
  }
  deriving (Show, Read, Eq, Ord)
$(mkLabels [''Tree])
-- nodeValue :: Tree a :-> a
-- nodeChildrenRefs :: Tree a :-> [IRef (Tree a)]
$(derive makeBinary ''Tree)
