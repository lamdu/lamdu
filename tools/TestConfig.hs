#!runghc -isrc -idist/build/autogen -XDeriveGeneric -XDeriveTraversable -XLambdaCase -XOverloadedStrings -XNoImplicitPrelude
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens.Operators
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy (Proxy(..))
import           Lamdu.Config (Config)
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Themes as Themes
import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import           System.IO (hPutStrLn, stderr)

import           Prelude

validate :: forall t. Aeson.FromJSON t => Proxy t -> FilePath -> IO ()
validate _ path =
    LBS.readFile path
    <&> Aeson.eitherDecode'
    >>= \case
    Left err -> hPutStrLn stderr $ "Failed to load " ++ path ++ ": " ++ err
    Right (_ :: t) -> putStrLn $ path ++ " parsed successfully"

main :: IO [()]
main = do
    configPath <- Paths.get Paths_Lamdu.getDataFileName "config.json"
    validate (Proxy :: Proxy Config) configPath
    Themes.getFiles >>= traverse (validate (Proxy :: Proxy Theme))
