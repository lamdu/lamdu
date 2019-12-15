-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, PolyKinds #-}
{-# LANGUAGE TypeApplications, FlexibleInstances, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, ConstraintKinds #-}
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

import           Control.Applicative (optional, (<|>))
import qualified Control.Lens as Lens
import           Control.Lens.Extended ((==>))
import           Data.Aeson ((.:), ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Hyper
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (ToNom(..), NominalDecl(..), NominalInst(..), NomVarTypes)
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.AST.Scheme (Scheme(..), QVars(..), QVarInstances(..), _QVarInstances)
import           Hyper.Type.Prune (Prune(..))
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
import           Lamdu.I18N.LangId (LangId(..))

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

class (FromJSON (Encoded a), ToJSON (Encoded a)) => JSON a where
    type Encoded a
    encode :: a -> Encoded a
    decode :: Encoded a -> AesonTypes.Parser a

type JSONTo encoded a = (JSON a, Encoded a ~ encoded)

class JSONKey k where
    encodeKey :: k -> Text
    decodeKey :: Text -> k
instance JSONKey [Char] where
    encodeKey = Text.pack
    decodeKey = Text.unpack
instance JSONKey Text where
    encodeKey = id
    decodeKey = id

instance (JSONKey key, Ord k, JSONTo key k, JSON v) => JSON (Map k v) where
    type Encoded (Map k v) = Aeson.Object
    encode m =
        Map.toList m
        & foldMap (\(k, v) -> encodeKey (encode k) ==> toValue v)
    decode obj =
        obj ^@.. Lens.itraversed
        & traverse decodeItem
        <&> Map.fromList
        where
            decodeItem (k, v) = (,) <$> decode (decodeKey k) <*> parseValue v

instance JSON a => JSON [a] where
    type Encoded [a] = Aeson.Array
    encode xs = xs <&> toValue & Vector.fromList
    decode arr = Vector.toList arr & traverse parseValue

toValue :: JSON a => a -> Aeson.Value
toValue = toJSON . encode

parseValue :: JSON a => Aeson.Value -> AesonTypes.Parser a
parseValue v = parseJSON v >>= decode

data NonEmptyArray a = NonEmptyArray
    { _neHead :: !a
    , _neTail :: !(Vector a)
    }

neSnoc :: Vector a -> a -> NonEmptyArray a
neSnoc prefix end =
    case Lens.uncons prefix of
    Nothing -> NonEmptyArray end mempty
    Just (x, xs) -> NonEmptyArray x (xs <> Vector.singleton end)

neUnsnoc :: NonEmptyArray a -> (Vector a, a)
neUnsnoc (NonEmptyArray hd tl)
    | Vector.null tl = (mempty, hd)
    | otherwise = (Vector.singleton hd <> Vector.init tl, Vector.last tl)

neArray :: NonEmptyArray a -> Vector a
neArray (NonEmptyArray hd tl) = Vector.cons hd tl

instance ToJSON a => ToJSON (NonEmptyArray a) where toJSON = toJSON . neArray
instance FromJSON a => FromJSON (NonEmptyArray a) where
    parseJSON =
        Aeson.withArray "NonEmptyArray" $ \arr ->
        case Lens.uncons arr of
        Nothing -> fail "NonEmptyArray parsing an empty array"
        Just (x, xs) ->
            NonEmptyArray <$> parseJSON x <*> traverse parseJSON xs

-- | Parse object based on field that should exist in it
decodeVariantObj ::
    String ->
    [(Text, Aeson.Object -> AesonTypes.Parser r)] ->
    Aeson.Object -> AesonTypes.Parser r
decodeVariantObj msg [] _ = "parseVariantObj of " <> msg <> " failed!" & fail
decodeVariantObj msg ((field, parser):rest) obj
    | Lens.has (Lens.ix field) obj = parser obj
    | otherwise = decodeVariantObj msg rest obj

instance JSON SchemaVersion where
    type Encoded SchemaVersion = Aeson.Object
    encode (SchemaVersion ver) = "schemaVersion" ==> toJSON ver
    decode obj = obj .: "schemaVersion" <&> SchemaVersion

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

instance JSON Entity where
    type Encoded Entity = Aeson.Object
    encode (EntitySchemaVersion x) = encode x
    encode (EntityRepl x) = encode x
    encode (EntityDef x) = encode x
    encode (EntityTag x) = encode x
    encode (EntityNominal x) = encode x
    encode (EntityLamVar x) = encode x
    decode =
        decodeVariantObj "entity"
        [ ("repl"          , fmap EntityRepl          . decode)
        , ("def"           , fmap EntityDef           . decode)
        , ("tagOrder"      , fmap EntityTag           . decode)
        , ("nom"           , fmap EntityNominal       . decode)
        , ("lamVar"        , fmap EntityLamVar        . decode)
        , ("schemaVersion" , fmap EntitySchemaVersion . decode)
        ]

instance JSON Meta.PresentationMode where
    type Encoded Meta.PresentationMode = Aeson.Value
    encode Meta.Verbose = Aeson.String "Verbose"
    encode (Meta.Operator l r) =
        "Operator" ==> array [toValue l, toValue r]
        & Aeson.Object
    decode (Aeson.String "Verbose") = pure Meta.Verbose
    decode x =
        Aeson.withObject "PresentationMode" parseObj x
        where
            parseObj =
                decodeVariantObj "PresentationMode"
                [ ("Operator", \o -> o .: "Operator" >>= decodeOperator)
                ]
            decodeOperator =
                Aeson.withArray "array of Operator tags" $
                \arr -> case Vector.toList arr of
                [l, r] -> Meta.Operator <$> parseValue l <*> parseValue r
                _ -> fail "Expecting two operator tags"

instance JSON Definition.FFIName where
    type Encoded Definition.FFIName = NonEmptyArray Text
    encode (Definition.FFIName modulePath name) =
        neSnoc (Vector.fromList modulePath) name
    decode arr =
        Definition.FFIName (Vector.toList initialVals) lastVal & pure
        where
            (initialVals, lastVal) = neUnsnoc arr

instance JSON Identifier where
    type Encoded Identifier = String
    encode = identHex
    decode = fromEither . identFromHex

decodeField :: JSON a => Aeson.Object -> Text -> AesonTypes.Parser a
decodeField obj name =
    case obj ^. Lens.at name of
    Nothing -> show name ++ " not in " ++ show obj & fail
    Just res -> parseValue res

encodeSquash ::
    (Eq (Encoded a), Monoid (Encoded a), JSON a) => Text -> a -> Aeson.Object
encodeSquash name x
    | res == mempty = mempty
    | otherwise = name ==> toJSON res
    where
        res = encode x

decodeSquashed ::
    (Monoid (Encoded a), JSON a) => Text -> Aeson.Object -> AesonTypes.Parser a
decodeSquashed name o
    | Lens.has (Lens.ix name) o = decodeField o name
    | otherwise = decode mempty

instance JSON T.Tag where
    type Encoded T.Tag = Aeson.Value
    encode tag
        | tag == Anchors.anonTag = Aeson.Null
        | otherwise = T.tagName tag & toValue
    decode Aeson.Null = pure Anchors.anonTag
    decode json = parseValue json <&> T.Tag

instance JSON (T.Var h) where
    type Encoded (T.Var h) = String
    encode = encode . T.tvName
    decode t = decode t <&> T.Var

instance ( JSON (h # T.Row)
         , JSONTo Aeson.Object (h # T.Type)
         ) => JSON (T.Row # h) where
    type Encoded (T.Row # h) = NonEmptyArray Aeson.Value
    encode T.REmpty = NonEmptyArray (Aeson.object []) mempty
    encode (T.RVar var) = NonEmptyArray ("rowVar" ==> toValue var & Aeson.Object) mempty
    encode (T.RExtend (RowExtend t v r)) =
        NonEmptyArray (Aeson.Object field) undefined -- (encode r)
        where
            field :: Aeson.Object
            field = encode v <> "rowTag" ==> toValue t

    decode (NonEmptyArray x xs) =
        case Lens.uncons xs of
        Nothing -> parseTail x
        Just {} ->
            uncurry RowExtend
            <$> parseField x
            <*> undefined -- decode xs
            <&> T.RExtend
        where
            parseTail =
                Aeson.withObject "Row.tail" $ \obj ->
                (obj .: "rowVar" >>= parseValue <&> T.RVar)
                <|> pure T.REmpty
            parseField =
                Aeson.withObject "field" $ \obj ->
                (,) <$> decodeField obj "rowTag" <*> decode obj

-- instance Aeson.FromJSONKey Identifier where
--     fromJSONKey = Aeson.FromJSONKeyTextParser (fromEither . identFromHex . Text.unpack)

-- instance Aeson.ToJSONKey Identifier where
--     toJSONKey = Aeson.toJSONKey & Lens.contramap identHex

deriving newtype instance JSON V.Var
deriving newtype instance JSON T.NominalId
-- deriving newtype instance ToJSON V.Var
-- deriving newtype instance ToJSON (T.Var a)
-- deriving newtype instance FromJSON (T.Var a)
-- deriving newtype instance FromJSON T.NominalId
-- deriving newtype instance Aeson.ToJSONKey (T.Var a)
-- deriving newtype instance Aeson.FromJSONKey (T.Var a)
-- deriving newtype instance Aeson.ToJSONKey V.Var
-- deriving newtype instance Aeson.FromJSONKey V.Var
-- deriving newtype instance Aeson.ToJSONKey T.NominalId
-- deriving newtype instance Aeson.FromJSONKey T.NominalId

-- type TypeRow (c :: * -> Constraint) h = (c (h # T.Type), c (h # T.Row))

instance (JSON (h # T.Row), JSON (h # T.Type)) =>
         JSON (T.Type # h) where
    type Encoded (T.Type # h) = Aeson.Object
    encode t =
        case t of
        T.TFun (FuncType a b) -> "funcParam" ==> toValue a <> "funcResult" ==> toValue b
        T.TRecord composite   -> "record" ==> toValue composite
        T.TVariant composite  -> "variant" ==> toValue composite
        T.TVar (T.Var name)   -> "typeVar" ==> toValue name
        T.TInst (NominalInst tId params) ->
            "nomId" ==> toValue (T.nomId tId) <>
            encodeSquash "nomTypeArgs" (params ^. T.tType . _QVarInstances) <>
            encodeSquash "nomRowArgs" (params ^. T.tRow . _QVarInstances)
    decode o =
        jsum
        [ FuncType
            <$> (decodeField o "funcParam")
            <*> (decodeField o "funcResult")
            <&> T.TFun
        , decodeField o "record" <&> T.TRecord
        , decodeField o "variant" <&> T.TVariant
        , decodeField o "typeVar" <&> T.Var <&> T.TVar
        , NominalInst
            <$> (decodeField o "nomId" <&> T.NominalId)
            <*> (T.Types
                <$> (decodeSquashed "nomTypeArgs" o <&> QVarInstances)
                <*> (decodeSquashed "nomRowArgs" o <&> QVarInstances)
                )
            <&> T.TInst
        ]

instance JSON T.RConstraints where
    type Encoded T.RConstraints = Aeson.Array
    encode (T.RowConstraints forbidden scopeLevel)
        | scopeLevel == mempty = Set.toList forbidden & Vector.fromList <&> encode
        | otherwise =
            -- We only encode top-level types, no skolem escape considerations...
            error "toJSON does not support inner-scoped types"
    decode arr =
        traverse decode arr <&> Vector.toList <&> Set.fromList
        <&> (`T.RowConstraints` mempty)

instance JSON (T.Types # QVars) where
    type Encoded (T.Types # QVars) = Aeson.Object
    encode (T.Types (QVars tvs) (QVars rvs)) =
        encodeSquash "typeVars" (Map.keys tvs)
        <> encodeSquash "rowVars" rvs
    decode obj =
        T.Types
        <$> (decodeSquashed "typeVars" obj <&> map T.Var
                <&> map (flip (,) mempty) <&> Map.fromList <&> QVars)
        <*> (decodeSquashed "rowVars" obj <&> QVars)

instance ( JSONTo Aeson.Object (varTypes # QVars)
         , JSON (h # typ)
         ) =>
         JSON (Scheme varTypes typ # h) where
    type Encoded (Scheme varTypes typ # h) = Aeson.Object
    encode (Scheme tvs typ) = "schemeType" ==> toValue typ <> encode tvs
    decode obj =
        do
            tvs <- decode obj
            typ <- decodeField obj "schemeType"
            Scheme tvs typ & pure

instance JSON V.Leaf where
    type Encoded V.Leaf = Aeson.Object
    encode =
        \case
        V.LHole -> l "hole"
        V.LRecEmpty -> l "recEmpty"
        V.LAbsurd -> l "absurd"
        V.LVar (V.Var var) -> "var" ==> toValue var
        V.LLiteral (V.PrimVal (T.NominalId primId) primBytes) ->
            "primId" ==> toValue primId <>
            "primBytes" ==> toJSON (BS.unpack (Hex.encode primBytes))
        V.LFromNom (T.NominalId nomId) ->
            "fromNomId" ==> toValue nomId
        where
            l x = x ==> Aeson.object []
    decode =
        decodeVariantObj "leaf"
        [ l "hole" V.LHole
        , l "recEmpty" V.LRecEmpty
        , l "absurd" V.LAbsurd
        , ("var", \o -> decodeField o "var" <&> V.Var <&> V.LVar)
        , ("primId",
            \obj ->
            do
                primId <- decodeField obj "primId" <&> T.NominalId
                bytesHex <- obj .: "primBytes"
                let (primBytes, remain) = Hex.decode (BS.pack bytesHex)
                BS.null remain & guard
                V.PrimVal primId primBytes & pure
            <&> V.LLiteral
          )
        , ("fromNomId", \o -> decodeField o "fromNomId" <&> T.NominalId <&> V.LFromNom)
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

instance JSONTo Aeson.Object (h # WithUUID) => JSON (WithUUID # h) where
    type Encoded (WithUUID # h) = Aeson.Object
    encode (Ann uuid body) = encode body <> "id" ==> toJSON uuid
    decode obj =
        Ann <$> (obj .: "id") <*> decode (obj & Lens.at "id" .~ Nothing)

instance ( JSON V.Var, JSON T.NominalId
         , JSON (h # HCompose Prune T.Type)
         , JSONTo Aeson.Object (h # V.Term)
         ) => JSON (V.Term # h) where
    type Encoded (V.Term # h) = Aeson.Object
    encode =
        \case
        V.BApp x -> encode x
        V.BLam x -> encode x
        V.BGetField x -> encode x
        V.BInject x -> encode x
        V.BToNom x -> encode x
        V.BLeaf x -> encode x
        V.BRecExtend x -> encodeRecExtend "extendTag" "extendVal" "extendRest" x
        V.BCase x -> encodeRecExtend "caseTag" "caseHandler" "caseRest" x
    decode obj =
        jsum
        [ decode obj <&> V.BApp
        , decode obj <&> V.BLam
        , decode obj <&> V.BGetField
        , decode obj <&> V.BInject
        , decode obj <&> V.BToNom
        , decode obj <&> V.BLeaf
        , parseRecExtend "extendTag" "extendVal" "extendRest" obj <&> V.BRecExtend
        , parseRecExtend "caseTag" "caseHandler" "caseRest" obj <&> V.BCase
        ]

instance JSON (h # V.Term) => JSON (V.App V.Term # h) where
    type Encoded (V.App V.Term # h) = Aeson.Object
    encode (V.App func arg) =
        "applyFunc" ==> toValue func <> "applyArg" ==> toValue arg
    decode obj = V.App <$> decodeField obj "applyFunc" <*> decodeField obj "applyArg"

instance ( JSON var, JSON (h # typ), JSON (h # term)
         ) => JSON (V.TypedLam var typ term # h) where
    type Encoded (V.TypedLam var typ term # h) = Aeson.Object
    encode (V.TypedLam varId paramType res) =
        "lamVar" ==> toValue varId <>
        "lamParamType" ==> toValue paramType <>
        "lamBody" ==> toValue res
    decode obj =
        V.TypedLam
        <$> decodeField obj "lamVar"
        <*> decodeField obj "lamParamType"
        <*> decodeField obj "lamBody"

instance JSON (h # V.Term) => JSON (V.GetField # h) where
    type Encoded (V.GetField # h) = Aeson.Object
    encode (V.GetField reco tag) =
        "getFieldRec" ==> toValue reco <>
        "getFieldName" ==> toValue tag
    decode obj =
        V.GetField
        <$> decodeField obj "getFieldRec"
        <*> decodeField obj "getFieldName"

encodeRecExtend ::
    (JSON key, JSON (h # val), JSON (h # rest)) =>
    Text -> Text -> Text -> RowExtend key val rest # h -> Aeson.Object
encodeRecExtend tagStr valStr restStr (RowExtend tag x rest) =
    tagStr ==> toValue tag <> valStr ==> toValue x <> restStr ==> toValue rest

parseRecExtend ::
    (JSON key, JSON (h # val), JSON (h # rest)) =>
    Text -> Text -> Text -> Aeson.Object ->
    AesonTypes.Parser (RowExtend key val rest # h)
parseRecExtend tagStr valStr restStr obj =
    RowExtend
    <$> decodeField obj tagStr
    <*> decodeField obj valStr
    <*> decodeField obj restStr

instance JSON (h # V.Term) => JSON (V.Inject # h) where
    type Encoded (V.Inject # h) = Aeson.Object
    encode (V.Inject tag x) =
        "injectTag" ==> toValue tag <>
        "injectVal" ==> toValue x
    decode obj =
        V.Inject
        <$> decodeField obj "injectTag"
        <*> decodeField obj "injectVal"

instance (JSON (h # term), JSON nomId) => JSON (ToNom nomId term # h) where
    type Encoded (ToNom nomId term # h) = Aeson.Object
    encode (ToNom nomId x) =
        "toNomId" ==> toValue nomId <>
        "toNomVal" ==> toValue x
    decode obj =
        ToNom
        <$> decodeField obj "toNomId"
        <*> decodeField obj "toNomVal"

instance JSON val => JSON (Definition.Expr val) where
    type Encoded (Definition.Expr val) = Aeson.Object
    encode (Definition.Expr x frozenDeps) =
        "val" ==> toValue x <>
        encodeSquash "frozenDeps" frozenDeps
    decode obj =
        Definition.Expr
        <$> decodeField obj "val"
        <*> decodeSquashed "frozenDeps" obj

instance JSON Deps where
    type Encoded Deps = Aeson.Object
    encode deps =
        encodeSquash "defTypes" (deps ^. depsGlobalTypes) <>
        encodeSquash "nominals" (deps ^. depsNominals)
    decode obj =
        Deps
        <$> decodeSquashed "defTypes" obj
        <*> decodeSquashed "nominals" obj

instance JSON val => JSON (Definition.Body val) where
    type Encoded (Definition.Body val) = Aeson.Object
    encode (Definition.BodyBuiltin name) = "builtin" ==> toValue name
    encode (Definition.BodyExpr defExpr) = encode defExpr
    decode obj =
        jsum
        [ decodeField obj "builtin" <&> Definition.BodyBuiltin
        , decode obj <&> Definition.BodyExpr
        ]

instance JSON ReplEntity where
    type Encoded ReplEntity = Aeson.Object
    encode (ReplEntity defExpr) = "repl" ==> toValue (defExpr :: Definition.Expr (Val UUID))
    decode obj = decodeField obj "repl" <&> ReplEntity

instance ( JSON (h # typ)
         , JSONTo Aeson.Object (NomVarTypes typ # QVars)
         ) => JSON (NominalDecl typ # h) where
    type Encoded (NominalDecl typ # h) = Aeson.Object
    encode (NominalDecl params nominalType) =
        "nomType" ==> toValue nominalType <> encode params
    decode obj =
        NominalDecl
        <$> decode obj
        <*> decodeField obj "nomType"

encodeTagged :: Text -> ((T.Tag, Identifier), Aeson.Object) -> Aeson.Object
encodeTagged idAttrName ((tag, ident), x) =
    x
    <> idAttrName ==> toValue ident
    <> "tag" ==> toValue tag

decodeTagged ::
    Text ->
    (Aeson.Object -> AesonTypes.Parser a) ->
    Aeson.Object -> AesonTypes.Parser ((T.Tag, Identifier), a)
decodeTagged idAttrName decoder obj =
    (,)
    <$> ( (,)
          <$> decodeField obj "tag"
          <*> decodeField obj idAttrName
        )
    <*> decoder obj

instance JSON val => JSON (Definition val (Meta.PresentationMode, T.Tag, V.Var)) where
    type Encoded (Definition val (Meta.PresentationMode, T.Tag, V.Var)) = Aeson.Object
    encode (Definition body scheme (presentationMode, tag, V.Var globalId)) =
        encodeTagged "def" ((tag, globalId), encode body)
        <> "typ" ==> undefined -- toValue scheme
        <> "defPresentationMode" ==> toValue presentationMode
    decode obj =
        undefined
        -- do
        --     ((tag, globalId), body) <- decodeTagged "def" decode obj
        --     presentationMode <- obj .: "defPresentationMode" >>= parseJSON
        --     scheme <- obj .: "typ" >>= parseJSON
        --     Definition body scheme (presentationMode, tag, V.Var globalId)
        --         & DefinitionEntity
        --         & pure

instance JSON DefinitionEntity where
    type Encoded DefinitionEntity = Aeson.Object
    encode (DefinitionEntity def) = undefined -- encode def
    decode obj = undefined {-decode-} obj <&> DefinitionEntity

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

instance JSON Text where
    type Encoded Text = Text
    encode = id
    decode = pure
deriving newtype instance JSON LangId

instance JSON Tag.TextsInLang where
    type Encoded Tag.TextsInLang = Aeson.Value
    encode (Tag.TextsInLang txt Nothing Nothing) = toValue txt
    encode (Tag.TextsInLang txt mAbb mDis) =
        "name" ==> toValue txt <>
        foldMap (("abbreviation" ==>) . toValue) mAbb <>
        foldMap (("disambiguationText" ==>) . toValue) mDis
        & Aeson.Object
    decode (Aeson.String txt) = pure (Tag.TextsInLang txt Nothing Nothing)
    decode json =
        Aeson.withObject "TextsInLang" f json
        where
            f o =
                Tag.TextsInLang
                <$> (o .: "name")
                <*> optional (o .: "abbreviation")
                <*> optional (o .: "disambiguationText")

instance JSON TagEntity where
    type Encoded TagEntity = Aeson.Object
    encode (TagEntity (T.Tag ident) (Tag order op names)) =
        (encodeTagOrder order <> "tag" ==> toValue ident)
        <> encodeSquash "names" names <> encodeSymbol op
    decode obj =
        TagEntity
        <$> (decodeField obj "tag" <&> T.Tag)
        <*>
        ( Tag
            <$> (obj .: "tagOrder")
            <*> decodeSymbol (obj ^. Lens.at "op")
            <*> decodeSquashed "names" obj
        )

instance JSON LamVarEntity where
    type Encoded LamVarEntity = Aeson.Object
    encode (LamVarEntity tag (V.Var ident)) =
        encodeTagged "lamVar" ((tag, ident), mempty)
    decode json =
        decodeTagged "lamVar" (\_ -> pure ()) json
        <&> \((tag, ident), ()) ->
        (LamVarEntity tag (V.Var ident))

instance JSON NominalEntity where
    type Encoded NominalEntity = Aeson.Object
    encode (NominalEntity tag (T.NominalId nomId) mNom) =
        undefined <>
        -- foldMap encode mNom <>
        "nom" ==> toValue nomId <>
        "tag" ==> toValue tag
    decode json =
        undefined
        -- decodeTagged "nom" (optional . decode) json
        <&> \((tag, ident), nom) -> NominalEntity tag (T.NominalId ident) nom

instance JSON (h # Pure) => JSON (Pure # h) where
    type Encoded (Pure # h) = Encoded (h # Pure)
    encode (Pure x) = encode x
    decode obj = decode obj <&> Pure

instance JSON (f # HCompose g h) => JSON (HCompose f g # h) where
    type Encoded (HCompose f g # h) = Encoded (f # HCompose g h)
    encode (HCompose x) = encode x
    decode obj = decode obj <&> HCompose

class PruneEncode json where
    type PruneEncoded json
    toPruneEncoded :: json -> PruneEncoded json
    fromPruneEncoded :: PruneEncoded json -> Maybe json

instance PruneEncode {-NonEmpty-}Aeson.Object where
    type PruneEncoded {-NonEmpty-}Aeson.Object = Aeson.Object
    toPruneEncoded = id
    fromPruneEncoded m
        | m == mempty = Nothing
        | otherwise = Just m

instance PruneEncode (NonEmptyArray Aeson.Value) where
    type PruneEncoded (NonEmptyArray Aeson.Value) = Aeson.Array
    toPruneEncoded = neArray
    fromPruneEncoded arr = Lens.uncons arr <&> uncurry NonEmptyArray

instance ( PruneEncode (Encoded (h # Prune))
         , FromJSON (PruneEncoded (Encoded (h # Prune)))
         , ToJSON (PruneEncoded (Encoded (h # Prune)))
         , Monoid (PruneEncoded (Encoded (h # Prune)))
         , JSON (h # Prune)
         ) => JSON (Prune # h) where
    type Encoded (Prune # h) = PruneEncoded (Encoded (h # Prune))
    encode Pruned = mempty
    encode (Unpruned x) = encode x & toPruneEncoded
    decode v =
        case fromPruneEncoded v of
        Nothing -> pure Pruned
        Just coded -> decode coded <&> Unpruned

-- instance FromJSON (HCompose WithUUID Prune # T.Row) where parseJSON = parseJSONObj "HCompose WithUUID Prune Row"
-- instance FromJSON (T.Row # HCompose WithUUID Prune) where parseJSON = Aeson.withArray "Row" decodeRow
-- instance FromJSON (WithUUID # HCompose Prune T.Type) where parseJSON = parseJSONObj "WithUUID PrunedType"
-- instance FromJSON (WithUUID # V.Term) where parseJSON = parseJSONObj "WithUUID Term"
-- instance FromJSON (HCompose Prune T.Row # WithUUID) where parseJSON = parseJSON <&> fmap HCompose
-- instance FromJSON (Prune # HCompose T.Row WithUUID) where
--     parseJSON =
--         decodePruneRow -- Prune # T.Row
-- instance JsonObject (T.Type # HCompose WithUUID Prune) where decode = decodeType
-- instance ToJSON (HCompose WithUUID Prune # T.Row) where toJSON = toJSONObj
-- instance ToJSON (T.Row # HCompose WithUUID Prune) where toJSON = toJSON . encodeRow
-- instance ToJSON (WithUUID # HCompose Prune T.Type) where toJSON = toJSONObj
-- instance ToJSON (WithUUID # V.Term) where toJSON = toJSONObj
-- instance JsonObject (HCompose Prune T.Row # WithUUID) where encode = encodeHComposeObj
-- instance JsonObject (HCompose Prune T.Type # WithUUID) where encode = encodeHComposeObj
-- instance ToJSON (HCompose T.Row WithUUID # Prune) where toJSON (HCompose row) = encodeRow row & toJSON
-- instance JsonObject (HCompose T.Type WithUUID # Prune) where encode = encodeHComposeObj
-- instance JsonObject (HCompose WithUUID Prune # T.Row) where encode = encodeHComposeObj
-- instance JsonObject (HCompose WithUUID Prune # T.Type) where encode = encodeHComposeObj
-- -- instance ToJSON (Prune # HCompose T.Row WithUUID) where encode = toJSON . encodePruneRow
-- instance JsonObject (Prune # HCompose T.Type WithUUID) where encode = encodePruneObject
-- instance JsonObject (WithUUID # HCompose Prune T.Row) where encode = encodeWithUUID
-- instance JsonObject (WithUUID # HCompose Prune T.Type) where encode = encodeWithUUID
-- instance JsonObject (WithUUID # V.Term) where encode = encodeWithUUID
