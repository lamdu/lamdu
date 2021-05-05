{-# LANGUAGE TypeFamilies, TypeApplications #-}

module Tests.Stdlib (test) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM_)
import qualified Data.Char as Char
import           Data.List (sort)
import           Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Hyper (htraverse_, (#>))
import qualified Hyper.Type.AST.Scheme as S
import           Lamdu.Calc.Definition (depsGlobalTypes)
import           Lamdu.Calc.Identifier (identHex)
import           Lamdu.Calc.Infer (alphaEq)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Export.JSON.Codec as JsonCodec
import           Lamdu.Data.Tag (tagTexts, tagSymbol, name, Symbol(..))
import           Lamdu.I18N.LangId (LangId(..))
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
    <&> concat
    >>= verifyTagNames . fmap (^. name)
    where
        verifyHasName (tagId, tag)
            | Lens.hasn't (tagTexts . traverse) tag && tag ^. tagSymbol == NoSymbol =
                fail ("stdlib tag with no name:" <> show tagId)
            | "" `elem` tag ^.. tagTexts . traverse . name =
                fail ("empty name for tag:" <> show tagId)
            | otherwise = tag ^.. tagTexts . Lens.ix (LangId "english") & pure

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
                [ (tag, tagRefTag ^. tagTexts . Lens.ix (LangId "english") . name)
                | (tag, tagRefTag) <- db ^.. Lens.folded . JsonCodec._EntityTag
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
    defs
    ^@.. traverse . Def.defBody . Def._BodyExpr . Def.exprFrozenDeps . depsGlobalTypes . Lens.itraversed
    & traverse_ (uncurry verifyGlobalType)
    where
        defTypes =
            defs <&> (\x -> (x ^. Def.defPayload . _3, (x ^. Def.defPayload . _2, x ^. Def.defType))) & Map.fromList
        verifyGlobalType var typ =
            case defTypes ^. Lens.at var of
            Nothing -> assertString ("Missing def referred in frozen deps: " ++ identHex (V.vvName var))
            Just (tag, x)
                | alphaEq x typ -> pure ()
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
                def ^.. Def.defBody . Def._BodyExpr . Def.exprFrozenDeps . depsGlobalTypes . traverse
                    & traverse_ verifyScheme
        verifyScheme (Pure s) = verifyTypeInScheme (s ^. S.sForAlls) (s ^. S.sTyp)

class VerifyTypeInScheme t where
    verifyTypeInScheme :: T.Types # S.QVars -> Pure # t -> IO ()

instance VerifyTypeInScheme T.Type where
    verifyTypeInScheme s (Pure (T.TVar v))
        | Lens.has (T.tType . S._QVars . Lens.ix v) s = pure ()
        | otherwise = assertString ("Type variable not declared " ++ show v)
    verifyTypeInScheme s (Pure t) = htraverse_ (Proxy @VerifyTypeInScheme #> verifyTypeInScheme s) t

instance VerifyTypeInScheme T.Row where
    verifyTypeInScheme s (Pure (T.RVar v))
        | Lens.has (T.tRow . S._QVars . Lens.ix v) s = pure ()
        | otherwise = assertString ("Row variable not declared " ++ show v)
    verifyTypeInScheme s (Pure t) = htraverse_ (Proxy @VerifyTypeInScheme #> verifyTypeInScheme s) t
