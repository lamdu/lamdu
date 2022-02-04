module Lamdu.Sugar.Names.NewTag (newTagName) where

import qualified Control.Lens as Lens
import           Data.Char (toLower)
import           Lamdu.Prelude

newTagName :: Text -> Text
newTagName t =
    ( case t ^? Lens._Cons of
        Just ('\'', r) -> r
        Just ('.', r) -> r
        Just ('{', r) -> r
        _ -> error ("unexpected new tag search term " <> show t)
    ) & Lens.ix 0 %~ toLower
