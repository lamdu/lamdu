module TestStdlib (test) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM_)
import qualified Data.Char as Char
import           Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Lamdu.Calc.Identifier (identHex)
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Export.JSON.Codec as JsonCodec
import qualified Lamdu.Infer as Infer
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertString)
import           Test.Lamdu.FreshDb (readFreshDb)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

test :: Test
test =
    testGroup "Stdlib"
    [ testCase "sensible-tags" verifyTagsTest
    , testCase "no-broken-defs" verifyNoBrokenDefsTest
    ]

verifyTagsTest :: IO ()
verifyTagsTest =
    readFreshDb
    <&> (^.. traverse . JsonCodec._EntityTag)
    >>= traverse verifyHasName
    >>= verifyTagNames
    where
        verifyHasName (_, Nothing, tag) = fail ("stdlib tag with no name:" <> show tag)
        verifyHasName (_, Just x, _) = pure x

verifyTagNames :: [Text] -> IO ()
verifyTagNames names =
    zipWithM_ verifyPair sorted (tail sorted)
    where
        sorted = sort names
        verifyPair x y
            | x == y = assertString ("duplicate tag name:" <> show x)
            | not (Text.isPrefixOf x y) = pure ()
            | Text.length x == 1 = pure ()
            | Set.member x prefixesWhitelist = pure ()
            | Set.member suffix suffixesWhitelist = pure ()
            | Lens.allOf (Lens.ix 0) Char.isUpper suffix = pure ()
            | otherwise =
                assertString ("inconsistent abbreviation detected: " <> show x <> ", " <> show y)
            where
                suffix = Text.drop (Text.length x) y

prefixesWhitelist :: Set Text
prefixesWhitelist =
    Set.fromList
    [ "by" -- prefix of "byte"
    , "connect" -- prefix of "connection"
    , "data" -- prefix of "database"
    , "head" -- prefix of "header"
    , "not" -- prefix of "nothing"
    , "or" -- prefix of "order"
    ]

suffixesWhitelist :: Set Text
suffixesWhitelist =
    Set.fromList
    [ "_" -- "sequence" => "sequence_"
    , "ed" -- "sort" => "sorted"
    , "s" -- "item" => "items"
    ]

verifyNoBrokenDefsTest :: IO ()
verifyNoBrokenDefsTest =
    do
        defs <- readFreshDb <&> (^.. traverse . JsonCodec._EntityDef)
        traverse_ verifyTag (defs <&> (^. Def.defPayload))
        defs <&> Def.defPayload %~ (^. Lens._3) & verifyDefs
    where
        verifyTag (_, tag, var)
            | tag == anonTag =
                assertString ("Definition with no tag: " ++ identHex (V.vvName var))
            | otherwise = pure ()

verifyDefs :: [Def.Definition v V.Var] -> IO ()
verifyDefs defs =
    defs ^.. traverse . Def.defBody . Def._BodyExpr . Def.exprFrozenDeps . Infer.depsGlobalTypes
    <&> Map.toList & concat
    & traverse_ (uncurry verifyGlobalType)
    where
        defTypes = defs <&> (\x -> (x ^. Def.defPayload, x ^. Def.defType)) & Map.fromList
        verifyGlobalType var typ =
            case defTypes ^. Lens.at var of
            Nothing -> assertString ("Missing def referred in frozen deps: " ++ showVar)
            Just x
                | Scheme.alphaEq x typ -> pure ()
                | otherwise ->
                    assertString
                    ("Frozen def type mismatch for " ++ showVar ++ ":\n" ++
                    prettyShow x ++ "\nvs\n" ++ prettyShow typ)
            where
                showVar = V.vvName var & identHex
