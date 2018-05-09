module TestJsRtsTags (test) where

import qualified Control.Lens as Lens
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import qualified Lamdu.Data.Export.JSON.Codec as JsonCodec
import           Test.Lamdu.FreshDb (readFreshDb)

import           Test.Lamdu.Prelude

test :: Test
test =
    do
        freshDbTags <-
            readFreshDb
            <&> (^.. traverse . JsonCodec._EntityTag . Lens._2 . Lens._Just)
            <&> Set.fromList
        rtsConfig <- readFile "js/export/rtsConfig.js"
        let (_:nameMapAndMore:_) = splitOn "var nameMap = {" rtsConfig
        let (nameMapTxt, _) = break (== '}') nameMapAndMore
        let nameMapItemTxts = splitOn "," nameMapTxt
        let nameMap =
                nameMapItemTxts
                <&> break (== ':')
                <&> Lens._1 %~ fromString . dropWhile (== ' ') . dropWhile (== '\n')
                <&> Lens._2 %~
                    fromString . takeWhile (/= '\'') .
                    dropWhile (== '\'') . dropWhile (== ' ') . tail
                & filter ((/= "data") . fst)
                    -- "data" is mapped to "__data" and apparently that's ok and necessary,
                    -- TODO: understand and document why.
                    -- See 21782dc1a6b6fbe9f2b371b192b8f8ac3840e9a9
                & Map.fromList
        let verifyRtsTag name
                | Set.member displayName freshDbTags = pure ()
                | otherwise =
                    assertString ("RTS uses tag which is not in the stdlib: " <> show name)
                where
                    displayName = Map.lookup name nameMap & fromMaybe name
        rtsTags <-
            readFile "js/anchors.js"
            <&> splitOn "conf.builtinTagName('"
            <&> tail
            <&> Lens.mapped %~ fromString . takeWhile (/= '\'')
        traverse_ verifyRtsTag rtsTags
        pure ()
    & testCase "js-rts-tags"
