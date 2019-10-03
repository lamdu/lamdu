import           Data.Aeson (eitherDecodeFileStrict')
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import           Lamdu.Data.Export.JSON.Migration (migrateAsNeeded)
import           System.Directory (renameFile)
import           System.Environment (getArgs)
import           System.FilePath ((<.>))

import           Lamdu.Prelude hiding ((<.>))

main :: IO ()
main =
    do
        paths <- getArgs
        for_ paths $ \path ->
            eitherDecodeFileStrict' path
            >>= \case
            Left err -> fail $ "decode " <> path <> ": " <> err
            Right value ->
                do
                    putStrLn $ "Migrating: " <> path
                    migratedJson <- migrateAsNeeded value
                    AesonPretty.encodePretty migratedJson
                        & BSL.writeFile migratedPath
                    renameFile path (path <.> "bk")
                    renameFile migratedPath path
                where
                    migratedPath = path <.> "migrated"
