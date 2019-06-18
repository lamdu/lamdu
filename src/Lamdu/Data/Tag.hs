{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Data.Tag
    ( Tag(..), tagOrder, tagOpName, tagNames
    , OpName(..), _NotAnOp, _OpUni, _OpDir, DirOp(..), opLeftToRight, opRightToLeft
    , LangNames(..), name, abbreviation, disambiguationText
    , getTagName
    ) where

import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import           Data.Binary
import           GUI.Momentu.Direction (Layout(..))
import qualified GUI.Momentu.Direction as Dir
import           Lamdu.I18N.LangId (LangId(..))

import           Lamdu.Prelude hiding ((.=))

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

data LangNames = LangNames
    { _name :: Text
    , _abbreviation :: Maybe Text
    , -- When a word has several meanings,
      -- the different meanings should have separate tags,
      -- as the word's translations into other languages may vary.
      -- Disambiguation-text can then be used to help pick the right tag.
      -- Similar to Wikipedia's disambiguation pages,
      -- such as https://en.wikipedia.org/wiki/Bark
      _disambiguationText :: Maybe Text
    } deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''LangNames

instance Aeson.ToJSON LangNames where
    toJSON (LangNames txt Nothing Nothing) = Aeson.toJSON txt
    toJSON (LangNames txt mAbb mDis) =
        concat
        [ ["name" .= txt]
        , ["abbreviation" .= abb | abb <- mAbb ^.. Lens._Just]
        , ["disambiguationText" .= dis | dis <- mDis ^.. Lens._Just]
        ] & Aeson.object

instance Aeson.FromJSON LangNames where
    parseJSON (Aeson.String txt) = pure (LangNames txt Nothing Nothing)
    parseJSON json =
        Aeson.withObject "LangNames" f json
        where
            f o =
                LangNames
                <$> (o .: "name")
                <*> optional (o .: "abbreviation")
                <*> optional (o .: "disambiguationText")

data Tag = Tag
    { _tagOrder :: Int
    , _tagOpName :: OpName
    , _tagNames :: Map LangId LangNames
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''Tag

getTagName ::
    (Has LangId env, Has Dir.Layout env) =>
    env -> Tag -> LangNames
getTagName env tag =
    case tag ^. tagOpName of
    NotAnOp -> n
    OpUni x -> LangNames x Nothing Nothing
    OpDir (DirOp l r) ->
        case env ^. has of
        LeftToRight -> opOrName l
        RightToLeft -> opOrName r
    where
        opOrName x
            | x == mempty = n -- No op for this direction
            | otherwise = LangNames x Nothing Nothing
        n =
            tag ^. tagNames . Lens.at (env ^. has)
            & fromMaybe fallback
        fallback =
            tag ^? tagNames . Lens.ix (LangId "english") & fromMaybe (LangNames "" Nothing Nothing)
