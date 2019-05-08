{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Data.Tag
    ( Tag(..), tagOrder, tagOpName, tagNames
    , OpName(..), _NotAnOp, _OpUni, _OpDir, DirOp(..), opLeftToRight, opRightToLeft
    , HasLanguageIdentifier(..)
    , getTagName
    ) where

import qualified Control.Lens as Lens
import           Data.Binary
import           GUI.Momentu.Direction (HasLayoutDir(..), Layout(..))

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

-- TODO: Appropriate module for this
class HasLanguageIdentifier env where
    languageIdentifier :: Lens' env Text

getTagName ::
    (HasLanguageIdentifier env, HasLayoutDir env) =>
    env -> Tag -> Text
getTagName env tag =
    case tag ^. tagOpName of
    NotAnOp -> name
    OpUni x -> x
    OpDir (DirOp l r) ->
        case env ^. layoutDir of
        LeftToRight -> opOrName l
        RightToLeft -> opOrName r
    where
        opOrName x
            | x == mempty = name -- No op for this direction
            | otherwise = x
        name =
            case tag ^. tagNames . Lens.at (env ^. languageIdentifier) of
            Just x -> x
            Nothing -> tag ^. tagNames . Lens.ix "english"
