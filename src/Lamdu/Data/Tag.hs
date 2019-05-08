{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Data.Tag
    ( Tag(..), tagOrder, tagOpName, tagNames
    , OpName(..), _NotAnOp, _OpUni, _OpDir, DirOp(..), opLeftToRight, opRightToLeft
    , getTagName
    ) where

import qualified Control.Lens as Lens
import           Data.Binary

import           Lamdu.Prelude

data DirOp = DirOp
    { _opLeftToRight :: Text
    , _opRightToLeft :: Text
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''DirOp

data OpName
    = NotAnOp
    | OpUni Text
    | OpDir DirOp
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makePrisms '' OpName

data Tag = Tag
    { _tagOrder :: Int
    , _tagOpName :: OpName
    , _tagNames :: Map Text Text
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''Tag

-- TODO: Currently this is fixed for English, make it usable for any language!
getTagName :: Tag -> Text
getTagName tag =
    case tag ^. tagOpName of
    NotAnOp -> tag ^. tagNames . Lens.ix "english"
    OpUni x -> x
    OpDir (DirOp x _) -> x
