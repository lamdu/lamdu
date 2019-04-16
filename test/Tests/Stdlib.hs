module Tests.Stdlib (test) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM_)
import qualified Data.Char as Char
import           Data.List (sort)
import           Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Lamdu.Calc.Identifier (identHex)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Export.JSON.Codec as JsonCodec
import qualified Lamdu.Infer as Infer
import           Test.Lamdu.FreshDb (readFreshDb)

import           Test.Lamdu.Prelude

test :: Test
test =
    testGroup "Stdlib"
    [ testCase "sensible-tags" verifyTagsTest
    , testCase "no-broken-defs" verifyNoBrokenDefsTest
    , testCase "schemes" verifySchemes
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
            | prefixesWhitelist ^. Lens.contains x = pure ()
            | suffixesWhitelist ^. Lens.contains suffix = pure ()
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
        db <- readFreshDb
        let tags =
                Map.fromList
                [ (tag, name)
                | (_, Just name, tag) <- db ^.. Lens.folded . JsonCodec._EntityTag
                ]
        let defs = db ^.. Lens.folded . JsonCodec._EntityDef
        traverse_ verifyTag (defs <&> (^. Def.defPayload))
        verifyDefs (tags !) defs
    where
        verifyTag (_, tag, var)
            | tag == anonTag =
                assertString ("Definition with no tag: " ++ identHex (V.vvName var))
            | otherwise = pure ()

verifyDefs :: (T.Tag -> Text) -> [Def.Definition v (presMode, T.Tag, V.Var)] -> IO ()
verifyDefs tagName defs =
    defs ^.. Lens.folded . Def.defBody . Def._BodyExpr . Def.exprFrozenDeps . Infer.depsGlobalTypes
    <&> Map.toList & concat
    & traverse_ (uncurry verifyGlobalType)
    where
        defTypes =
            defs <&> (\x -> (x ^. Def.defPayload . _3, (x ^. Def.defPayload . _2, x ^. Def.defType))) & Map.fromList
        verifyGlobalType var typ =
            case defTypes ^. Lens.at var of
            Nothing -> assertString ("Missing def referred in frozen deps: " ++ identHex (V.vvName var))
            Just (tag, x)
                | Scheme.alphaEq x typ -> pure ()
                | otherwise ->
                    assertString
                    ("Frozen def type mismatch for " ++ show (tagName tag) ++ ":\n" ++
                    prettyShow x ++ "\nvs\n" ++ prettyShow typ)

verifySchemes :: IO ()
verifySchemes =
    do
        db <- readFreshDb
        db ^.. Lens.folded . JsonCodec._EntityDef & traverse_ verifyDef
    where
        verifyDef def =
            do
                def ^. Def.defType & verifyScheme
                def ^.. Def.defBody . Def._BodyExpr . Def.exprFrozenDeps . Infer.depsGlobalTypes . traverse
                    & traverse_ verifyScheme
        verifyScheme s = verifyType (s ^. Scheme.schemeForAll) (s ^. Scheme.schemeType)
        verifyType s (T.TVar v)
            | Infer.typeVars s ^. Lens.contains v = pure ()
            | otherwise = assertString ("Type variable not declared " ++ show v)
        verifyType s (T.TFun a b) = verifyType s a >> verifyType s b
        verifyType s (T.TInst _ params) = traverse_ (verifyType s) params
        verifyType s (T.TRecord r) = verifyRow s r
        verifyType s (T.TVariant r) = verifyRow s r
        verifyRow s (T.RVar v)
            | Infer.rowVars s ^. Lens.contains v = pure ()
            | otherwise = assertString ("Row variable not declared " ++ show v)
        verifyRow s (T.RExtend _ t r) = verifyType s t >> verifyRow s r
        verifyRow _ T.REmpty{} = pure ()
