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
import           Hyper
import           Hyper.Syntax.Nominal (nScheme)
import qualified Hyper.Syntax.Scheme as S
import           Lamdu.Calc.Definition (depsGlobalTypes)
import           Lamdu.Calc.Identifier (identHex)
import           Lamdu.Calc.Infer (alphaEq)
import           Lamdu.Calc.Lens (valGlobals, valTags)
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
    , testCase "dead-tags" verifyUsedTags
    , testCase "no-broken-defs" verifyNoBrokenDefsTest
    , testCase "schemes" verifySchemes
    ]

verifyUsedTags :: IO ()
verifyUsedTags =
    do
        db <- readFreshDb
        let unused =
                Set.difference
                (Set.fromList (db ^.. traverse . JsonCodec._EntityTag . _1))
                (Set.fromList (db ^.. traverse >>= usedTags))
                ^.. Lens.folded
                <&> identHex . T.tagName
                & filter (`notElem` whitelist)
        unless (null unused) (fail ("unused tags: " <> unwords unused))
    where
        whitelist =
            [ "42493a66756e6374696f6e0000000000" -- func
            , "42493a7265636f726400000000000000" -- rec
            , "42493a756e6974000000000000000000" -- unit
            , "42493a76617269616e74000000000000" -- variant
            , "42493a766f6964000000000000000000" -- void
            -- Useful words:
            , "f38fc647fb4b73e35d1ffec481a44dd8" -- "prev"
            , "f526d897cab5429fb66ebbe0b4b8f34e" -- "window"
            ]

usedTags :: JsonCodec.Entity -> [T.Tag]
usedTags (JsonCodec.EntityLamVar t _) = [t]
usedTags (JsonCodec.EntityDef (Def.Definition b s p)) =
    b ^.. Def._BodyExpr . Def.expr . valTags
    <> s ^.. _Pure . S.sTyp . typeTags
    <> p ^.. _2
usedTags (JsonCodec.EntityNominal t _ d) =
    t :
    either
    ( hfoldMap
        ( \case
            HWitness T.W_Types_Type -> map T.Tag . (^.. S._QVars . Lens.ifolded . Lens.asIndex . T._Var)
            HWitness T.W_Types_Row -> map T.Tag . (^.. S._QVars . Lens.ifolded . Lens.asIndex . T._Var)
        )
    )
    (^.. _Pure . nScheme . S.sTyp . typeTags) d
usedTags _ = []

typeTags :: Lens.Traversal' (Pure # T.Type) T.Tag
typeTags f =
    htraverse
    ( \case
        HWitness T.W_Type_Type -> typeTags f
        HWitness T.W_Type_Row -> rowTags f
        HWitness (T.E_Type_NominalInst_NominalId_Types (HWitness T.W_Types_Type)) -> typeTags f
        HWitness (T.E_Type_NominalInst_NominalId_Types (HWitness T.W_Types_Row)) -> rowTags f
    )
    & _Pure

rowTags :: Lens.Traversal' (Pure # T.Row) T.Tag
rowTags f =
    (_Pure . T._RExtend)
    (\(V.RowExtend t v r) -> V.RowExtend <$> f t <*> typeTags f v <*> rowTags f r)

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

verifyDefs :: (T.Tag -> Text) -> [Def.Definition (Annotated a # V.Term) (presMode, T.Tag, V.Var)] -> IO ()
verifyDefs tagName defs =
    defs
    ^@.. Lens.traverse . Lens.filteredBy (Def.defPayload . _3)
        <.> Def.defBody . Def._BodyExpr .
            Lens.filteredBy (Def.expr . Lens.to (Set.fromList . (^.. valGlobals mempty)))
        <.> Def.exprFrozenDeps . depsGlobalTypes . Lens.itraversed
    & traverse_ (uncurry verifyGlobalType)
    where
        defTypes =
            defs <&> (\x -> (x ^. Def.defPayload . _3, (x ^. Def.defPayload . _2, x ^. Def.defType))) & Map.fromList
        verifyGlobalType (inDef, (usedDefs, var)) typ
            | usedDefs ^. Lens.contains var =
                case defTypes ^. Lens.at var of
                Nothing ->
                    "Missing def referred in frozen deps " <> identHex (V.vvName var) <> inText & assertString
                Just (tag, x)
                    | alphaEq x typ -> pure ()
                    | otherwise ->
                        "Frozen def type mismatch" <> inText <> " for " <> show (tagName tag) <> ":\n" ++
                        prettyShow x <> "\nvs\n" <> prettyShow typ
                        & assertString
            | otherwise = "Stale frozen dep: " <> identHex (V.vvName var) <> inText & assertString
            where
                inText = " in " <> identHex (V.vvName inDef)

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
