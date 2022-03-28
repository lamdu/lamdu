module Data.Aeson.Config
    ( load
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.FastWriter (WriterT)
import qualified Control.Monad.Trans.FastWriter as Writer
import           Data.Aeson (FromJSON(..), Result(..), eitherDecode', fromJSON, object)
import           Data.Aeson.Lens (_Object, _String, key, values)
import           Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import           System.FilePath (takeDirectory, isRelative, (</>))

import           Lamdu.Prelude

-- | Left argument overrides right argument (like `<>` for `Map`)
override :: Value -> Value -> Value
override (Object x) (Object y) =
    Map.unionWith override (toMap x) (toMap y) ^@.. Lens.ifolded & object
    where
        toMap = Map.fromList . (^@.. Lens.ifolded)
override x _ = x

importsKey :: Text
importsKey = "imports"

imports :: FilePath -> Value -> WriterT [FilePath] IO Value
imports dirName x =
    x ^.. key importsKey . values . _String
    <&> Text.unpack
    <&> addDir
    & traverse load
    <&> foldl override (x & _Object . Lens.at importsKey .~ Nothing)
    where
        addDir path
            | isRelative path = dirName </> path
            | otherwise = path

load :: FromJSON a => FilePath -> WriterT [FilePath] IO a
load path =
    Writer.tell [path] *>
    liftIO (LBS.readFile path)
    <&> eitherDecode'
    >>= either (error . mappend msg) (imports (takeDirectory path))
    <&> fromJSON
    >>=
    \case
    Error x -> error (msg <> x)
    Success x -> pure x
    where
        msg = "Failed to parse config file contents at " ++ show path ++ ": "
