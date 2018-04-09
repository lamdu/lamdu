module TestJsRtsTags (test) where

import qualified Control.Lens as Lens
import           Data.List.Split (splitOn)
import qualified Data.Set as Set
import           Data.String (IsString(..))
import qualified Lamdu.Data.Export.JSON.Codec as JsonCodec
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertString)
import           Test.Lamdu.FreshDb (readFreshDb)

import           Lamdu.Prelude

test :: Test
test =
    do
        freshDbTags <-
            readFreshDb
            <&> (^.. traverse . JsonCodec._EntityTag . Lens._2 . Lens._Just)
            <&> Set.fromList
        let verifyRtsTag name
                | Set.member name freshDbTags = pure ()
                | otherwise =
                    assertString ("RTS uses tag which is not in the stdlib: " <> show name)
        rtsTags <-
            readFile "js/anchors.js"
            <&> splitOn "conf.builtinTagName('"
            <&> tail
            <&> Lens.mapped %~ fromString . takeWhile (/= '\'')
        traverse_ verifyRtsTag rtsTags
        pure ()
    & testCase "js-rts-tags"
