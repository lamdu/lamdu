module Tests.JsRtsTags (test) where

import qualified Control.Lens as Lens
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import qualified Lamdu.Data.Export.JSON.Codec as JsonCodec
import           Lamdu.Data.Tag (tagTexts, name)
import           Lamdu.I18N.LangId (LangId(..))
import           Lamdu.Paths (readDataFile)
import           Test.Lamdu.FreshDb (readFreshDb)

import           Test.Lamdu.Prelude

test :: TestTree
test =
    do
        freshDbTags <-
            readFreshDb
            <&> (^.. traverse . JsonCodec._EntityTag . _2 . tagTexts
                 . Lens.ix (LangId "english") . name)
            <&> Set.fromList
        rtsConfig <- readDataFile "js/export/rtsConfig.js"
        let (_:nameMapAndMore:_) = splitOn "var nameMap = {" rtsConfig
        let (nameMapTxt, _) = break (== '}') nameMapAndMore
        let nameMapItemTxts = splitOn "," nameMapTxt
        let nameMap =
                nameMapItemTxts
                <&> break (== ':')
                <&> _1 %~ fromString . dropWhile (== ' ') . dropWhile (== '\n')
                <&> _2 %~
                    fromString . takeWhile (/= '\'') .
                    dropWhile (== '\'') . dropWhile (== ' ') . tail
                & filter ((/= "data") . fst)
                    -- "data" is mapped to "__data" and apparently that's ok and necessary,
                    -- TODO: understand and document why.
                    -- See 21782dc1a6b6fbe9f2b371b192b8f8ac3840e9a9
                & Map.fromList
        let verifyRtsTag x
                | freshDbTags ^. Lens.contains displayName = pure ()
                | otherwise =
                    assertString ("RTS uses tag which is not in the stdlib: " <> show x)
                where
                    displayName = Map.lookup x nameMap & fromMaybe x
        rtsTags <-
            readDataFile "js/anchors.js"
            <&> splitOn "conf.builtinTagName('"
            <&> tail
            <&> Lens.mapped %~ fromString . takeWhile (/= '\'')
        traverse_ verifyRtsTag rtsTags
    & testCase "js-rts-tags"
