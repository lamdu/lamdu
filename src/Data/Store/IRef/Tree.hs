{-# LANGUAGE TemplateHaskell #-}

module Data.Store.IRef.Tree (Tree(..), atNodeValue, atNodeChildrenRefs)
where

import Data.Binary       (Binary(..))
import Data.Store.IRef   (IRef)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import qualified Data.AtFieldTH as AtFieldTH

data Tree a = Node {
  nodeValue :: a,
  nodeChildrenRefs :: [IRef (Tree a)]
  }
  deriving (Show, Read, Eq, Ord)
AtFieldTH.make ''Tree
derive makeBinary ''Tree
