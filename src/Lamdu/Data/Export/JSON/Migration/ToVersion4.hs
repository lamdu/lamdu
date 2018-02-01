{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Data.Export.JSON.Migration.ToVersion4 (migrate) where

import qualified Control.Lens as Lens
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import           Numeric.Utils (encodeHex)

import           Lamdu.Prelude

version :: Integer -> Aeson.Value
version x =
    mempty
    & Lens.at "schemaVersion" ?~ Aeson.toJSON x
    & Aeson.Object

collectTags :: Aeson.Value -> Either Text (Map Text (Set Text))
collectTags (Aeson.Object obj) =
    case (,) <$> obj ^. Lens.at "tag" <*> obj ^. Lens.at "name" of
    Just (Aeson.String tag, Aeson.String name) ->
        mempty & Lens.at name %~ mappend (Just (Set.singleton tag)) & Right
    Just _ -> Left "Malformed 'tag' node"
    Nothing -> Right mempty
collectTags _ = Right mempty

objToTransform :: Lens.Traversal' Aeson.Object Aeson.Value
objToTransform = Lens.ix "lamId" `Lens.failing` Lens.ix "def" `Lens.failing` Lens.ix "nom"

collectNames :: Aeson.Value -> Either Text (Set Text)
collectNames (Aeson.Object obj) | Lens.has objToTransform obj =
    case obj ^. Lens.at "name" of
    Just (Aeson.String t) -> Set.singleton t & Right
    Just _ -> Left ("Object has non-string name" <> Text.pack (show obj))
    _ -> Right mempty
collectNames _ = Right mempty

migrateEntity :: Map Text Text -> Aeson.Value -> Either Text Aeson.Value
migrateEntity nameToTag (Aeson.Object obj) | Lens.has objToTransform obj =
    case obj ^. Lens.at "name" of
    Nothing -> Right "00000000000000000000000000000000"
    Just (Aeson.String name) -> nameToTag ^. Lens.ix name & Right
    Just{} -> Left "Non-text name!"
    <&>
    \tag ->
    obj
    & Lens.at "name" .~ Nothing
    & Lens.at "tag" ?~ Aeson.toJSON tag
    & Aeson.Object
migrateEntity _ x@Aeson.Object{} = Right x
migrateEntity _ _ = Left "Expecting object"

newTagEntity :: Text -> Text -> Aeson.Value
newTagEntity name tag =
    mempty
    & Lens.at "tagOrder" ?~ Aeson.Number 0
    & Lens.at "tag" ?~ Aeson.toJSON tag
    & Lens.at "name" ?~ Aeson.toJSON name
    & Aeson.Object

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate (Aeson.Array vals)
    | Vector.head vals == version 3 =
        do
            tags <- traverse collectTags vals <&> (^. traverse)
            names <- traverse collectNames vals <&> (^. traverse)
            let newTags = filter (`Map.notMember` tags) (names ^.. Lens.folded) <&> addTag
            let allTags = (tags <&> (^?! Lens.folded)) <> Map.fromList newTags
            traverse (migrateEntity allTags) (Vector.tail vals)
                <&> mappend (newTags <&> uncurry newTagEntity & Vector.fromList)
        <&> (,) (version 4)
        <&> (Lens._Cons #)
        <&> Aeson.Array
    where
        addTag name =
            ( name
            , fromString (encodeHex (SBS.take 16 (SHA256.hash (encodeUtf8 name))))
            )
migrate _ = Left "top-level should be array"
