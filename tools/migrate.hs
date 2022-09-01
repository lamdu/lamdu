import           Data.Aeson (eitherDecodeFileStrict')
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import           Lamdu.Data.Export.JSON.Migration (migrateAsNeeded)
import           System.Directory (renameFile)
import           System.FilePath ((<.>))
import qualified Options.Applicative as P

import           Lamdu.Prelude hiding ((<.>))

parsePath :: P.Parser FilePath
parsePath =
    P.metavar "JSONPATH" <> P.help "path to exported json file from older version"
    & P.strArgument

parseArgs :: IO [FilePath]
parseArgs =
    P.info
    (P.helper <*> P.many parsePath)
    (P.progDesc "Migrate - migrate old export files to newer versions")
    & P.execParser

main :: IO ()
main =
    do
        paths <- parseArgs
        for_ paths $ \path ->
            eitherDecodeFileStrict' path
            >>= \case
            Left err -> fail $ "decode " <> path <> ": " <> err
            Right value ->
                do
                    putStrLn $ "Migrating: " <> path
                    (_, migratedJson) <- migrateAsNeeded putStrLn value
                    AesonPretty.encodePretty migratedJson
                        & BSL.writeFile migratedPath
                    renameFile path (path <.> "bk")
                    renameFile migratedPath path
                where
                    migratedPath = path <.> "migrated"
