-- | Processing export files

{-# LANGUAGE LambdaCase #-}
module Lamdu.Data.Export.JSON.Process (process) where

import           Control.Lens.Operators
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import           System.IO (Handle, stdin, stdout)

hProcess :: Handle -> Handle -> ([Codec.Entity] -> IO [Codec.Entity]) -> IO ()
hProcess inHandle outHandle f =
    BSL.hGetContents inHandle
    <&> Aeson.eitherDecode
    >>= \case
        Left err -> fail err
        Right entities ->
            f entities
            <&> AesonPretty.encodePretty
            >>= BSL.hPutStr outHandle

process :: ([Codec.Entity] -> IO [Codec.Entity]) -> IO ()
process = hProcess stdin stdout
