module Test.Lamdu.FreshDb (readFreshDb) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Lamdu.Data.Export.JSON.Codec (Entity)
import qualified Lamdu.Paths as Paths

import           Lamdu.Prelude

readFreshDb :: IO [Entity]
readFreshDb =
    Paths.getDataFileName "freshdb.json"
    >>= LBS.readFile <&> Aeson.eitherDecode
    >>= either fail pure
    <&> Aeson.fromJSON
    >>=
    \case
    Aeson.Error str -> fail str
    Aeson.Success x -> pure x
