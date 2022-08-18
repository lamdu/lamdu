{-# LANGUAGE TypeApplications #-}
module Tests.Config (test) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.DeepSeq (deepseq)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Config (load)
import qualified Data.Aeson.Diff as AesonDiff
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Aeson.Lens (_Object, _String, key)
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import           Data.List (sort, group)
import           Data.Proxy (asProxyTypeOf)
import           Data.Text (unpack)
import           GUI.Momentu (ModKey)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.MetaKey as MetaKey
import           Lamdu.Config (Config)
import           Lamdu.Config.Folder (HasConfigFolder)
import qualified Lamdu.Config.Folder as Folder
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.I18N.Language (Language)
import qualified Lamdu.Paths as Paths
import           System.FilePath (takeFileName)
import qualified System.Info as SysInfo

import           Test.Lamdu.Prelude

test :: TestTree
test =
    testGroup "config-tests"
    [ testCase "config-parse" (verifyJson (Proxy @(Config Text)) metaKeyRoundTrip "config.json")
    , testCase "themes-parse" (verifyConfigFolder (Proxy @Theme))
    , testCase "languages-parse" (verifyConfigFolder (Proxy @Language))
    , testCase "sprites" verifySprites
    , languagesDupTest
    ]
    where
        assertParse :: Text -> ModKey
        assertParse str =
            MetaKey.parse SysInfo.os str
            & either (error . (("failed to parse MetaKey: " ++ show str ++ ": ") ++)) id
        metaKeyRoundTrip config =
            (config
            <&> assertParse
            <&> MetaKey.format SysInfo.os)
            `deepseq` ()

verifySprites :: IO ()
verifySprites =
    Folder.getSelections (Proxy @Theme)
    >>= traverse (Folder.selectionToPath (Proxy @Theme))
    >>= traverse loadJsonPath
    >>= traverse_ verifySpritesOf
    where
        verifySpritesOf theme =
            theme ^. Theme.sprites
            & traverse Folder.spritePath
            >>= traverse_ Draw.openSprite

verifyConfigFolder ::
    (HasConfigFolder a, Aeson.FromJSON a, Aeson.ToJSON a) =>
    Proxy a -> IO ()
verifyConfigFolder p =
    Folder.getSelections p
    >>= traverse (Folder.selectionToPath p)
    >>= traverse_ (verifyJson p id)

verifyJson ::
    (Aeson.FromJSON a, Aeson.ToJSON a) =>
    Proxy a -> (a -> b) -> FilePath -> IO ()
verifyJson proxy postProcess jsonPath =
    do
        json <- loadJsonPath jsonPath
        case Aeson.fromJSON json <&> (`asProxyTypeOf` proxy) of
            Aeson.Error msg -> assertString ("Failed decoding " <> jsonPath <> " from json: " <> msg)
            Aeson.Success val
                | rejson == json -> pure ()
                | otherwise ->
                    assertString ("json " <> jsonPath <> " contains unexpected data:\n" <>
                        LBSChar.unpack (AesonPretty.encodePretty (Aeson.toJSON (AesonDiff.diff rejson json))))
                where
                    rejson = postProcess val `seq` Aeson.toJSON val

loadJsonPath :: Aeson.FromJSON a => FilePath -> IO a
loadJsonPath path =
    do
        (json, deps) <- Paths.getDataFileName path >>= Writer.runWriterT . load
        if elem path deps || elem path (deps <&> takeFileName)
            then pure json
            else fail ("config file not its own dependency " <> path <> " not in " <> show deps)

languagesDupTest :: TestTree
languagesDupTest =
    Folder.getSelections (Proxy @Language)
    >>= traverse (Folder.selectionToPath (Proxy @Language))
    >>= traverse_ checkDups
    & testCase "languages-dup"

checkDups :: FilePath -> IO ()
checkDups path =
    do
        json <- loadJsonPath path
        let allTexts =
                (json :: Aeson.Value) ^..
                key "texts" .
                Lens.folding (Lens.universeOf (_Object . traverse)) .
                _String
        group (sort allTexts) ^.. traverse . Lens.ix 1 & traverse_ onDup
    where
        onDup text =
            assertString ("duplicated text: " <> unpack text)
            & unless (text `elem` whitelist)
        whitelist =
            [ "Modifica" -- Both "edit" and "modify" in Italian
            , "Ripeti" -- "repeat" in Italian but also best option for "undo"
            ]
