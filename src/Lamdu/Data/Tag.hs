{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Lamdu.Data.Tag
    ( Tag(..), tagOrder, tagOpName, tagNames
    , OpName(..), _NotAnOp, _OpUni, _OpDir, DirOp(..), opLeftToRight, opRightToLeft
    , getTagName
    ) where

import qualified Control.Lens as Lens
import           Data.Binary
import           Data.Has (Has(..))
import           GUI.Momentu.Direction (Layout(..))
import qualified GUI.Momentu.Direction as Dir
import           Lamdu.I18N.LangId (LangId(..))

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
    , _tagNames :: Map LangId Text
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''Tag

getTagName ::
    (Has LangId env, Has Dir.Layout env) =>
    env -> Tag -> Text
getTagName env tag =
    case tag ^. tagOpName of
    NotAnOp -> name
    OpUni x -> x
    OpDir (DirOp l r) ->
        case env ^. has of
        LeftToRight -> opOrName l
        RightToLeft -> opOrName r
    where
        opOrName x
            | x == mempty = name -- No op for this direction
            | otherwise = x
        name =
            tag ^. tagNames . Lens.at (env ^. has)
            & fromMaybe (tag ^. tagNames . Lens.ix (LangId "english"))
