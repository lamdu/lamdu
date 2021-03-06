{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Data.Tag
    ( Tag(..), tagOrder, tagSymbol, tagTexts
    , Symbol(..), _NoSymbol, _UniversalSymbol, _DirectionalSymbol, DirOp(..), opLeftToRight, opRightToLeft
    , TextsInLang(..), name, abbreviation, disambiguationText
    , IsOperator(..)
    , getTagName
    ) where

import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import           GUI.Momentu.Direction (Layout(..))
import qualified GUI.Momentu.Direction as Dir
import           Lamdu.I18N.LangId (LangId(..))

import           Lamdu.Prelude hiding ((.=))

data DirOp = DirOp
    { _opLeftToRight :: !Text
    , _opRightToLeft :: !Text
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''DirOp

data Symbol
    = NoSymbol
    | UniversalSymbol Text
    | DirectionalSymbol DirOp
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makePrisms '' Symbol

data TextsInLang = TextsInLang
    { _name :: !Text
    , _abbreviation :: !(Maybe Text)
    , -- When a word has several meanings,
      -- the different meanings should have separate tags,
      -- as the word's translations into other languages may vary.
      -- Disambiguation-text can then be used to help pick the right tag.
      -- Similar to Wikipedia's disambiguation pages,
      -- such as https://en.wikipedia.org/wiki/Bark
      _disambiguationText :: !(Maybe Text)
    } deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''TextsInLang

data IsOperator = NotAnOperator | IsAnOperator
    deriving (Eq, Ord)

instance Aeson.ToJSON TextsInLang where
    toJSON (TextsInLang txt Nothing Nothing) = Aeson.toJSON txt
    toJSON (TextsInLang txt mAbb mDis) =
        concat
        [ ["name" .= txt]
        , ["abbreviation" .= abb | abb <- mAbb ^.. Lens._Just]
        , ["disambiguationText" .= dis | dis <- mDis ^.. Lens._Just]
        ] & Aeson.object

instance Aeson.FromJSON TextsInLang where
    parseJSON (Aeson.String txt) = pure (TextsInLang txt Nothing Nothing)
    parseJSON json =
        Aeson.withObject "TextsInLang" f json
        where
            f o =
                TextsInLang
                <$> (o .: "name")
                <*> optional (o .: "abbreviation")
                <*> optional (o .: "disambiguationText")

data Tag = Tag
    { _tagOrder :: !Int
    , _tagSymbol :: !Symbol
    , _tagTexts :: !(Map LangId TextsInLang)
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass Binary
Lens.makeLenses ''Tag

getTagName ::
    (Has LangId env, Has Dir.Layout env) =>
    env -> Tag -> (IsOperator, TextsInLang)
getTagName env tag =
    case tag ^. tagSymbol of
    NoSymbol -> (NotAnOperator, n)
    UniversalSymbol x -> (IsAnOperator, TextsInLang x Nothing Nothing)
    DirectionalSymbol (DirOp l r) ->
        case env ^. has of
        LeftToRight -> l
        RightToLeft -> r
        & opOrName
    where
        opOrName x
            | x == mempty = (NotAnOperator, n) -- No op for this direction
            | otherwise = (IsAnOperator, TextsInLang x Nothing Nothing)
        n =
            tag ^. tagTexts . Lens.at (env ^. has)
            & fromMaybe fallback
        fallback =
            tag ^? tagTexts . Lens.ix (LangId "english") & fromMaybe (TextsInLang "" Nothing Nothing)
