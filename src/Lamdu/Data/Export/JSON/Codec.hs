-- | JSON encoder/decoder for Lamdu types
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, PolyKinds, TypeApplications, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Export.JSON.Codec
    ( TagOrder
    , SchemaVersion(..), _SchemaVersion
    , ReplEntity(..), _ReplEntity
    , DefinitionEntity(..), _DefinitionEntity
    , NominalEntity(..), nominalTag, nominalEntityId, nominalDecl
    , TagEntity(..), tagId, tagData
    , LamVarEntity(..), lamVarId, lamVarVar
    , Entity(..), _EntitySchemaVersion, _EntityRepl, _EntityDef, _EntityTag, _EntityNominal, _EntityLamVar
    ) where

import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Control.Lens.Extended ((==>))
import           Data.Aeson ((.:), ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import qualified Data.Vector as Vector
import           Hyper
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (ToNom(..), NominalDecl(..), NominalInst(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.AST.Scheme (Scheme(..), QVars(..), QVarInstances(..), _QVarInstances)
import           Hyper.Type.Prune (Prune(..), _Unpruned)
import           Lamdu.Calc.Definition
import           Lamdu.Calc.Identifier (Identifier, identHex, identFromHex)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Data.Tag (Tag(..))
import qualified Lamdu.Data.Tag as Tag

import           Lamdu.Prelude hiding ((.=))

array :: [Aeson.Value] -> Aeson.Value
array = Aeson.Array . Vector.fromList

toEither :: AesonTypes.Parser a -> Either String a
toEither parser = AesonTypes.parseEither (\() -> parser) ()

fromEither :: Either String a -> AesonTypes.Parser a
fromEither = either fail pure

jsum :: [AesonTypes.Parser a] -> AesonTypes.Parser a
jsum parsers =
    parsers <&> toEither
    <&> swapEither & sequence <&> unlines & swapEither
    & fromEither
    where
        swapEither (Left x) = Right x
        swapEither (Right x) = Left x

class JsonObject a where
    parseObject :: Aeson.Object -> AesonTypes.Parser a
    toObject :: a -> Aeson.Object

parseJSONObj :: JsonObject a => String -> Aeson.Value -> AesonTypes.Parser a
parseJSONObj msg = Aeson.withObject msg parseObject

toJSONObj :: JsonObject a => a -> Aeson.Value
toJSONObj = Aeson.Object . toObject

data NominalEntity = NominalEntity
    { _nominalTag :: !T.Tag
    , _nominalEntityId :: !T.NominalId
    , _nominalDecl :: !(Maybe (Pure # NominalDecl T.Type))
    }
Lens.makeLenses ''NominalEntity

data TagEntity = TagEntity
    { _tagId :: !T.Tag
    , _tagData :: !Tag
    }
Lens.makeLenses ''TagEntity

data LamVarEntity = LamVarEntity
    { _lamVarId :: !T.Tag
    , _lamVarVar :: !V.Var
    }
Lens.makeLenses ''LamVarEntity

type TagOrder = Int

newtype SchemaVersion = SchemaVersion Int deriving (Eq, Ord, Show)
Lens.makePrisms ''SchemaVersion

instance ToJSON SchemaVersion where toJSON = toJSONObj
instance JsonObject SchemaVersion where
    toObject (SchemaVersion ver) = "schemaVersion" ==> toJSON ver
    parseObject = (.: "schemaVersion") <&> fmap SchemaVersion

newtype ReplEntity = ReplEntity (Definition.Expr (Val UUID))
Lens.makePrisms ''ReplEntity

newtype DefinitionEntity =
    DefinitionEntity (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
Lens.makePrisms ''DefinitionEntity

data Entity
    = EntitySchemaVersion !SchemaVersion
    | EntityRepl !ReplEntity
    | EntityDef !DefinitionEntity
    | EntityTag !TagEntity
    | EntityNominal !NominalEntity
    | EntityLamVar !LamVarEntity
Lens.makePrisms ''Entity

instance ToJSON Entity where
    toJSON (EntitySchemaVersion x) = toJSON x
    toJSON (EntityRepl x) = toJSON x
    toJSON (EntityDef x) = toJSON x
    toJSON (EntityTag x) = toJSON x
    toJSON (EntityNominal x) = toJSON x
    toJSON (EntityLamVar x) = toJSON x

-- | Parse object based on field that should exist in it
decodeVariantObj ::
    String ->
    [(Text, Aeson.Object -> AesonTypes.Parser r)] ->
    Aeson.Object -> AesonTypes.Parser r
decodeVariantObj msg [] _ = "parseVariantObj of " <> msg <> " failed!" & fail
decodeVariantObj msg ((field, parser):rest) obj
    | Lens.has (Lens.ix field) obj = parser obj
    | otherwise = decodeVariantObj msg rest obj

instance FromJSON Entity where parseJSON = parseJSONObj "Entity"
instance JsonObject Entity where
    parseObject =
        decodeVariantObj "entity"
        [ ("repl"          , fmap EntityRepl          . parseObject)
        , ("def"           , fmap EntityDef           . parseObject)
        , ("tagOrder"      , fmap EntityTag           . parseObject)
        , ("nom"           , fmap EntityNominal       . parseObject)
        , ("lamVar"        , fmap EntityLamVar        . parseObject)
        , ("schemaVersion" , fmap EntitySchemaVersion . parseObject)
        ]

instance ToJSON Meta.PresentationMode where
    toJSON Meta.Verbose = Aeson.String "Verbose"
    toJSON (Meta.Operator l r) =
        "Operator" ==> array [toJSON l, toJSON r]
        & Aeson.Object

instance FromJSON Meta.PresentationMode where
    parseJSON (Aeson.String "Verbose") = pure Meta.Verbose
    parseJSON x =
        Aeson.withObject "PresentationMode" parseObj x
        where
            parseObj =
                decodeVariantObj "PresentationMode"
                [ ("Operator", \o -> o .: "Operator" >>= decodeOperator)
                ]
            decodeOperator =
                Aeson.withArray "array of Operator tags" $
                \arr -> case Vector.toList arr of
                [l, r] -> Meta.Operator <$>  parseJSON l <*> parseJSON r
                _ -> fail "Expecting two operator tags"

instance ToJSON Definition.FFIName where
    toJSON (Definition.FFIName modulePath name) = modulePath ++ [name] & toJSON

instance FromJSON Definition.FFIName where
    parseJSON =
        Aeson.withArray "array of FFIName components" $
        \arr -> case Vector.toList arr of
        [] -> fail "Expecting at least one FFIName component"
        xs ->
            Definition.FFIName
            <$> traverse parseJSON (init xs)
            <*> parseJSON (last xs)

instance ToJSON Identifier where
    toJSON = toJSON . identHex

instance FromJSON Identifier where
    parseJSON json =
        parseJSON json
        <&> identFromHex
        >>= fromEither

encodeSquash :: (Eq a, Monoid a, ToJSON a) => Text -> a -> Aeson.Object
encodeSquash name x
    | x == mempty = mempty
    | otherwise = name ==> toJSON x

decodeSquashed ::
    (FromJSON j, Monoid a) =>
    Text -> (j -> AesonTypes.Parser a) -> Aeson.Object -> AesonTypes.Parser a
decodeSquashed name decode o
    | Lens.has (Lens.ix name) o = o .: name >>= decode
    | otherwise = pure mempty

instance ToJSON T.Tag where
    toJSON tag
        | tag == Anchors.anonTag = Aeson.Null
        | otherwise = T.tagName tag & toJSON

instance FromJSON T.Tag where
    parseJSON Aeson.Null = pure Anchors.anonTag
    parseJSON json = parseJSON json <&> T.Tag

instance ToJSON (Pure # T.Row) where
    toJSON =
        array . go
        where
            go (Pure T.REmpty) = []
            go (Pure (T.RVar (T.Var name))) =
                ["rowVar" ==> toJSON name & Aeson.Object]
            go (Pure (T.RExtend (RowExtend t v r))) =
                -- TODO: use toObject (or _Object is not guaranteed to do anything)
                (toJSON v & _Object . Lens.at "rowTag" ?~ toJSON t) : go r

instance FromJSON (Pure # T.Row) where
    parseJSON (Aeson.Array vec) =
        case items ^? Lens._Snoc >>= _2 (^? _Object . Lens.ix "rowVar") of
        Just (elems, restVar) ->
            foldr field (parseJSON restVar <&> Pure . T.RVar . T.Var) elems
        _ -> foldr field (Pure T.REmpty & pure) items
        where
            items = Vector.toList vec
            field x rest =
                Aeson.withObject "RowField" ?? x $
                \o ->
                RowExtend
                <$> (o .: "rowTag" >>= parseJSON)
                <*> parseJSON x
                <*> rest
                <&> Pure . T.RExtend
    parseJSON x = fail ("malformed row" <> show x)

instance Aeson.FromJSONKey Identifier where
    fromJSONKey = Aeson.FromJSONKeyTextParser (fromEither . identFromHex . Text.unpack)

instance Aeson.ToJSONKey Identifier where
    toJSONKey = Aeson.toJSONKey & Lens.contramap identHex

deriving newtype instance Aeson.ToJSON (T.Var a)
deriving newtype instance Aeson.FromJSON (T.Var a)
deriving newtype instance Aeson.ToJSONKey (T.Var a)
deriving newtype instance Aeson.FromJSONKey (T.Var a)
deriving newtype instance Aeson.ToJSONKey V.Var
deriving newtype instance Aeson.FromJSONKey V.Var
deriving newtype instance Aeson.ToJSONKey T.NominalId
deriving newtype instance Aeson.FromJSONKey T.NominalId

instance ToJSON (Pure # T.Type) where toJSON = toJSONObj
instance FromJSON (Pure # T.Type) where parseJSON = parseJSONObj "Type"
instance JsonObject (Pure # T.Type) where
    toObject t =
        case t ^. _Pure of
        T.TFun (FuncType a b) -> "funcParam" ==> toJSON a <> "funcResult" ==> toJSON b
        T.TRecord composite   -> "record" ==> toJSON composite
        T.TVariant composite  -> "variant" ==> toJSON composite
        T.TVar (T.Var name)   -> "typeVar" ==> toJSON name
        T.TInst (NominalInst tId params) ->
            "nomId" ==> toJSON (T.nomId tId) <>
            encodeSquash "nomTypeArgs" (params ^. T.tType . _QVarInstances) <>
            encodeSquash "nomRowArgs" (params ^. T.tRow . _QVarInstances)
    parseObject o =
        jsum
        [ FuncType
            <$> (o .: "funcParam" >>= parseJSON)
            <*> (o .: "funcResult" >>= parseJSON)
            <&> T.TFun
        , o .: "record" >>= parseJSON <&> T.TRecord
        , o .: "variant" >>= parseJSON <&> T.TVariant
        , o .: "typeVar" >>= parseJSON <&> T.Var <&> T.TVar
        , NominalInst
            <$> (o .: "nomId" >>= parseJSON <&> T.NominalId)
            <*> (T.Types
                <$> (decodeSquashed "nomTypeArgs" parseJSON o <&> QVarInstances)
                <*> (decodeSquashed "nomRowArgs" parseJSON o <&> QVarInstances)
                )
            <&> T.TInst
        ]
        <&> (_Pure #)

instance ToJSON T.RConstraints where
    toJSON (T.RowConstraints forbidden scopeLevel)
        | scopeLevel == mempty = toJSON forbidden
        | otherwise =
            -- We only encode top-level types, no skolem escape considerations...
            error "toJSON does not support inner-scoped types"

instance FromJSON T.RConstraints where
    parseJSON =
        Aeson.withArray "Composite Constraints" $ \arr ->
        traverse parseJSON arr <&> Vector.toList <&> Set.fromList
        <&> (`T.RowConstraints` mempty)

instance JsonObject (T.Types # QVars) where
    toObject (T.Types (QVars tvs) (QVars rvs)) =
        encodeSquash "typeVars" (Map.keysSet tvs)
        <> encodeSquash "rowVars" rvs
    parseObject obj =
        T.Types
        <$> ( decodeSquashed "typeVars"
            ( \tvs ->
                parseJSON tvs
                >>= traverse parseJSON
                <&> map (\name -> (T.Var name, mempty))
                <&> Map.fromList
            ) obj <&> QVars
            )
        <*> (decodeSquashed "rowVars" parseJSON obj <&> QVars)

instance ToJSON (Pure # T.Scheme) where toJSON = toJSONObj
instance JsonObject (Pure # T.Scheme) where
    toObject (Pure (Scheme tvs typ)) = "schemeType" ==> toJSON typ <> toObject tvs

instance FromJSON (Pure # T.Scheme) where
    parseJSON =
        Aeson.withObject "scheme" $ \obj ->
        do
            tvs <- parseObject obj
            typ <- obj .: "schemeType" >>= parseJSON
            _Pure # Scheme tvs typ & pure

instance JsonObject V.Leaf where
    toObject =
        \case
        V.LHole -> l "hole"
        V.LRecEmpty -> l "recEmpty"
        V.LAbsurd -> l "absurd"
        V.LVar (V.Var var) -> "var" ==> toJSON var
        V.LLiteral (V.PrimVal (T.NominalId primId) primBytes) ->
            "primId" ==> toJSON primId <>
            "primBytes" ==> toJSON (BS.unpack (Hex.encode primBytes))
        V.LFromNom (T.NominalId nomId) ->
            "fromNomId" ==> toJSON nomId
        where
            l x = x ==> Aeson.object []
    parseObject =
        decodeVariantObj "leaf"
        [ l "hole" V.LHole
        , l "recEmpty" V.LRecEmpty
        , l "absurd" V.LAbsurd
        , ("var", \o -> o .: "var" >>= parseJSON <&> V.Var <&> V.LVar)
        , ("primId",
            \obj ->
            do
                primId <- obj .: "primId" >>= parseJSON <&> T.NominalId
                bytesHex <- obj .: "primBytes"
                let (primBytes, remain) = Hex.decode (BS.pack bytesHex)
                BS.null remain & guard
                V.PrimVal primId primBytes & pure
            <&> V.LLiteral
          )
        , ("fromNomId", \o -> o .: "fromNomId" >>= parseJSON <&> T.NominalId <&> V.LFromNom)
        ]
        where
            l key v =
                ( key
                , \obj ->
                    obj .: key >>=
                    \case
                    Aeson.Object x | x == mempty -> pure v
                    x -> fail ("bad val for leaf " ++ show x)
                )

type WithUUID = Ann (Const UUID)

encodeWithUUID :: JsonObject (h # WithUUID) => WithUUID # h -> Aeson.Object
encodeWithUUID (Ann uuid body) = toObject body <> "id" ==> toJSON uuid

decodeWithUUID ::
    JsonObject (h # WithUUID) =>
    Aeson.Object -> AesonTypes.Parser (WithUUID # h)
decodeWithUUID obj = Ann <$> (obj .: "id") <*> parseObject obj

decodePruned ::
    JsonObject (a # HCompose b Prune) =>
    Aeson.Object -> AesonTypes.Parser (HCompose Prune a # b)
decodePruned obj
    | (obj & Lens.at "id" .~ Nothing) == mempty =
        _HCompose # Pruned & pure
    | otherwise =
        parseObject obj <&> (hcomposed _Unpruned #)

instance ToJSON (WithUUID # V.Term) where toJSON = toJSONObj
instance FromJSON (WithUUID # V.Term) where parseJSON = parseJSONObj "WithUUID Term"
instance JsonObject (WithUUID # V.Term) where
    toObject = encodeWithUUID
    parseObject = decodeWithUUID

type PrunedType = HCompose Prune T.Type

instance ToJSON (WithUUID # PrunedType) where toJSON = toJSONObj
instance FromJSON (WithUUID # PrunedType) where parseJSON = parseJSONObj "WithUUID PrunedType"
instance JsonObject (WithUUID # PrunedType) where
    toObject = encodeWithUUID
    parseObject = decodeWithUUID

instance JsonObject (V.Term # WithUUID) where
    toObject =
        \case
        V.BApp (V.App func arg) ->
            "applyFunc" ==> toJSON func <>
            "applyArg" ==> toJSON arg
        V.BLam (V.TypedLam (V.Var varId) paramType res) ->
            "lamVar" ==> toJSON varId <>
            "lamParamType" ==> toJSON paramType <>
            "lamBody" ==> toJSON res
        V.BGetField (V.GetField reco tag) ->
            "getFieldRec" ==> toJSON reco <>
            "getFieldName" ==> toJSON tag
        V.BRecExtend (RowExtend tag x rest) ->
            "extendTag" ==> toJSON tag <>
            "extendVal" ==> toJSON x <>
            "extendRest" ==> toJSON rest
        V.BInject (V.Inject tag x) ->
            "injectTag" ==> toJSON tag <>
            "injectVal" ==> toJSON x
        V.BCase (RowExtend tag handler restHandler) ->
            "caseTag" ==> toJSON tag <>
            "caseHandler" ==> toJSON handler <>
            "caseRest" ==> toJSON restHandler
        V.BToNom (ToNom (T.NominalId nomId) x) ->
            "toNomId" ==> toJSON nomId <>
            "toNomVal" ==> toJSON x
        V.BLeaf x -> toObject x
    parseObject obj =
        jsum
        [ parseObject obj <&> V.BApp
        , parseObject obj <&> V.BLam
        , parseObject obj <&> V.BGetField
        , parseRecExtend "extendTag" "extendVal" "extendRest" obj <&> V.BRecExtend
        , parseObject obj <&> V.BInject
        , parseRecExtend "caseTag" "caseHandler" "caseRest" obj <&> V.BCase
        , parseObject obj <&> V.BToNom
        , parseObject obj <&> V.BLeaf
        ]

instance JsonObject (V.App V.Term # WithUUID) where
    parseObject obj = V.App <$> obj .: "applyFunc" <*> obj .: "applyArg"

instance JsonObject (V.TypedLam V.Var (PrunedType) V.Term # WithUUID) where
    parseObject obj =
        V.TypedLam
        <$> (obj .: "lamVar" >>= parseJSON <&> V.Var)
        <*> (obj .: "lamParamType")
        <*> (obj .: "lamBody")

instance JsonObject (V.GetField # WithUUID) where
    parseObject obj =
        V.GetField
        <$> (obj .: "getFieldRec")
        <*> (obj .: "getFieldName" >>= parseJSON)

parseRecExtend ::
    (FromJSON key, FromJSON (h # val), FromJSON (h # rest)) =>
    Text -> Text -> Text -> Aeson.Object ->
    AesonTypes.Parser (RowExtend key val rest # h)
parseRecExtend tagStr valStr restStr obj =
    RowExtend
    <$> (obj .: tagStr >>= parseJSON)
    <*> obj .: valStr
    <*> obj .: restStr

instance JsonObject (V.Inject # WithUUID) where
    parseObject obj =
        V.Inject
        <$> (obj .: "injectTag" >>= parseJSON)
        <*> (obj .: "injectVal")

instance JsonObject (ToNom T.NominalId V.Term # WithUUID) where
    parseObject obj =
        ToNom
        <$> (obj .: "toNomId" >>= parseJSON <&> T.NominalId)
        <*> (obj .: "toNomVal")

instance FromJSON (HCompose WithUUID Prune # T.Row) where
    parseJSON = undefined

parseType ::
    FromJSON (h # T.Row) =>
    Aeson.Object -> AesonTypes.Parser (T.Type # h)
parseType obj =
    jsum
    [ obj .: "record" >>= parseJSON <&> T.TRecord
    ]

instance JsonObject (T.Type # HCompose WithUUID Prune) where
    parseObject = parseType

instance JsonObject (PrunedType # WithUUID) where
    parseObject = decodePruned
    toObject (HCompose Pruned) = mempty
    toObject (HCompose (Unpruned (HCompose (T.TRecord (HCompose row))))) =
        "record" ==> array (go row)
        where
            go (Ann uuid (HCompose b)) =
                Aeson.Object ("rowId" ==> toJSON uuid <> x) : xs
                where
                    (x, xs) =
                        case b of
                        Pruned -> (mempty, [])
                        Unpruned (HCompose T.REmpty) -> (mempty, [])
                        Unpruned
                            (HCompose
                                (T.RExtend
                                    (RowExtend t
                                        (HCompose (Ann fId (HCompose Pruned)))
                                        (HCompose r)))) ->
                            ( "rowTag" ==> toJSON t
                                <> "id" ==> toJSON fId
                            , go r
                            )
                        Unpruned _ -> error "TODO"
    toObject (HCompose (Unpruned _)) = error "TODO"

instance JsonObject (Definition.Expr (Val UUID)) where
    toObject (Definition.Expr x frozenDeps) =
        "val" ==> toJSON x <>
        encodeSquash "frozenDeps" encodedDeps
        where
            encodedDeps =
                encodeSquash "defTypes" (frozenDeps ^. depsGlobalTypes) <>
                encodeSquash "nominals" (frozenDeps ^. depsNominals)
    parseObject obj =
        Definition.Expr
        <$> (obj .: "val" >>= parseJSON)
        <*> decodeSquashed "frozenDeps" (Aeson.withObject "deps" decodeDeps) obj
        where
            decodeDeps o =
                Deps
                <$> decodeSquashed "defTypes" parseJSON o
                <*> decodeSquashed "nominals" parseJSON o

instance JsonObject (Definition.Body (Val UUID)) where
    toObject (Definition.BodyBuiltin name) = "builtin" ==> toJSON name
    toObject (Definition.BodyExpr defExpr) = toObject defExpr
    parseObject obj =
        jsum
        [ obj .: "builtin" >>= parseJSON <&> Definition.BodyBuiltin
        , parseObject obj <&> Definition.BodyExpr
        ]

instance ToJSON ReplEntity where toJSON = toJSONObj
instance JsonObject ReplEntity where
    toObject (ReplEntity defExpr) = "repl" ==> Aeson.Object (toObject defExpr)
    parseObject obj =
        obj .: "repl" >>= Aeson.withObject "defExpr" parseObject <&> ReplEntity

instance FromJSON (Pure # NominalDecl T.Type) where
    parseJSON = parseJSONObj "Pure NominalDecl"
instance ToJSON (Pure # NominalDecl T.Type) where toJSON = toJSONObj
instance JsonObject (Pure # NominalDecl T.Type) where
    toObject (Pure (NominalDecl params nominalType)) =
        "nomType" ==> toJSON (_Pure # nominalType) <> toObject params
    parseObject obj =
        NominalDecl
        <$> parseObject obj
        <*> (obj .: "nomType" >>= parseJSON <&> (^. _Pure))
        <&> (_Pure #)

encodeTagged :: Text -> (a -> Aeson.Object) -> ((T.Tag, Identifier), a) -> Aeson.Object
encodeTagged idAttrName encoder ((tag, ident), x) =
    encoder x
    <> idAttrName ==> toJSON ident
    <> "tag" ==> toJSON tag

decodeTagged ::
    Text ->
    (Aeson.Object -> AesonTypes.Parser a) ->
    Aeson.Object -> AesonTypes.Parser ((T.Tag, Identifier), a)
decodeTagged idAttrName decoder obj =
    (,)
    <$> ( (,)
          <$> (obj .: "tag" >>= parseJSON)
          <*> (obj .: idAttrName >>= parseJSON)
        )
    <*> decoder obj

instance ToJSON DefinitionEntity where toJSON = toJSONObj
instance JsonObject DefinitionEntity where
    toObject (DefinitionEntity
               (Definition body scheme (presentationMode, tag, V.Var globalId))) =
        encodeTagged "def" toObject ((tag, globalId), body)
        <> "typ" ==> toJSON scheme
        <> "defPresentationMode" ==> toJSON presentationMode
    parseObject obj =
        do
            ((tag, globalId), body) <- decodeTagged "def" parseObject obj
            presentationMode <- obj .: "defPresentationMode" >>= parseJSON
            scheme <- obj .: "typ" >>= parseJSON
            Definition body scheme (presentationMode, tag, V.Var globalId)
                & DefinitionEntity
                & pure

encodeTagOrder :: TagOrder -> Aeson.Object
encodeTagOrder tagOrder = "tagOrder" ==> toJSON tagOrder

encodeSymbol :: Tag.Symbol -> Aeson.Object
encodeSymbol Tag.NoSymbol = mempty
encodeSymbol (Tag.UniversalSymbol x) = "op" ==> Aeson.String x
encodeSymbol (Tag.DirectionalSymbol (Tag.DirOp l r)) =
    "op" ==> array [Aeson.String l, Aeson.String r]

decodeSymbol :: Maybe Aeson.Value -> AesonTypes.Parser Tag.Symbol
decodeSymbol Nothing = pure Tag.NoSymbol
decodeSymbol (Just (Aeson.String x)) = Tag.UniversalSymbol x & pure
decodeSymbol (Just (Aeson.Array x)) =
    case x ^.. traverse of
    [Aeson.String l, Aeson.String r] ->
        Tag.DirOp l r & Tag.DirectionalSymbol & pure
    _ -> fail ("unexpected op names:" <> show x)
decodeSymbol x = fail ("unexpected op name: " <> show x)

instance ToJSON TagEntity where toJSON = toJSONObj
instance JsonObject TagEntity where
    toObject (TagEntity (T.Tag ident) (Tag order op names)) =
        (encodeTagOrder order <> "tag" ==> toJSON ident)
        <> encodeSquash "names" names <> encodeSymbol op
    parseObject obj =
        TagEntity
        <$> (obj .: "tag" >>= parseJSON <&> T.Tag)
        <*>
        ( Tag
            <$> (obj .: "tagOrder")
            <*> decodeSymbol (obj ^. Lens.at "op")
            <*> decodeSquashed "names" parseJSON obj
        )

instance ToJSON LamVarEntity where
    toJSON (LamVarEntity tag (V.Var ident)) =
        encodeTagged "lamVar" (const mempty) ((tag, ident), ()) & Aeson.Object

instance JsonObject LamVarEntity where
    parseObject json =
        decodeTagged "lamVar" (\_ -> pure ()) json
        <&> \((tag, ident), ()) ->
        (LamVarEntity tag (V.Var ident))

instance ToJSON NominalEntity where toJSON = toJSONObj
instance JsonObject NominalEntity where
    toObject (NominalEntity tag (T.NominalId nomId) mNom) =
        foldMap toObject mNom
        & Lens.at "nom" ?~ toJSON nomId
        & Lens.at "tag" ?~ toJSON tag
    parseObject json =
        decodeTagged "nom" (optional . parseObject) json
        <&> \((tag, ident), nom) -> NominalEntity tag (T.NominalId ident) nom
