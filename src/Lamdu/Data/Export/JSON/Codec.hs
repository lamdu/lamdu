-- | JSON encoder/decoder for Lamdu types
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, PolyKinds, TypeApplications, FlexibleInstances #-}
module Lamdu.Data.Export.JSON.Codec
    ( TagOrder
    , Entity(..), _EntitySchemaVersion, _EntityRepl, _EntityDef, _EntityTag, _EntityNominal, _EntityLamVar
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((==>))
import           Data.Aeson ((.:), ToJSON(..), FromJSON(..))
import           Data.Aeson.Lens (_Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
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

type Encoder a = a -> Aeson.Value
type Decoder a = Aeson.Value -> AesonTypes.Parser a

type TagOrder = Int

data Entity
    = EntitySchemaVersion Int
    | EntityRepl (Definition.Expr (Val UUID))
    | EntityDef (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
    | EntityTag T.Tag Tag
    | EntityNominal T.Tag T.NominalId (Maybe (Pure # NominalDecl T.Type))
    | EntityLamVar T.Tag V.Var
Lens.makePrisms ''Entity

instance ToJSON Entity where
    toJSON (EntitySchemaVersion ver) = encodeSchemaVersion ver
    toJSON (EntityRepl x) = encodeRepl x
    toJSON (EntityDef def) = toJSON def
    toJSON (EntityTag tid tdata) = encodeNamedTag (tid, tdata)
    toJSON (EntityNominal tag nomId nom) = encodeTaggedNominal ((tag, nomId), nom)
    toJSON (EntityLamVar tag var) = encodeTaggedLamVar (tag, var)

instance FromJSON Entity where
    parseJSON =
        decodeVariant "entity"
        [ ("repl", fmap EntityRepl . decodeRepl)
        , ("def", fmap EntityDef . decodeDef)
        , ("tagOrder", fmap (uncurry EntityTag) . decodeNamedTag)
        , ("nom", fmap (\((tag, nomId), nom) -> EntityNominal tag nomId nom) . decodeTaggedNominal)
        , ("lamVar", fmap (uncurry EntityLamVar) . decodeTaggedLamVar)
        , ("schemaVersion", fmap EntitySchemaVersion . decodeSchemaVersion)
        ]

instance ToJSON Meta.PresentationMode where
    toJSON Meta.Verbose = Aeson.String "Verbose"
    toJSON (Meta.Operator l r) =
        "Operator" ==> array [toJSON l, toJSON r]
        & Aeson.Object

instance FromJSON Meta.PresentationMode where
    parseJSON (Aeson.String "Verbose") = pure Meta.Verbose
    parseJSON x =
        decodeVariant "Type"
        [ ("Operator", \o -> o .: "Operator" >>= decodeOperator)
        ] x
        where
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

encodeIdentMap :: ToJSON a => (k -> Identifier) -> Encoder (Map k a)
encodeIdentMap getIdent m =
    m
    & Map.mapKeys (identHex . getIdent)
    & toJSON

decodeIdentMap ::
    (FromJSON j, Ord k) =>
    (Identifier -> k) -> (j -> AesonTypes.Parser a) -> Decoder (Map k a)
decodeIdentMap fromIdent decode json =
    parseJSON json
    <&> Map.toList
    >>= Lens.traverse %%~ decodePair
    <&> Map.fromList
    where
        decodePair (k, v) =
            (,)
            <$> (identFromHex k & fromEither <&> fromIdent)
            <*> decode v

encodeSquash ::
    (Eq a, Monoid a, ToJSON j) =>
    Text -> (a -> j) -> a -> Aeson.Object
encodeSquash name encode x
    | x == mempty = mempty
    | otherwise = name ==> toJSON (encode x)

-- | Parse object based on containing some traversal
decodeVariantObj ::
    String ->
    [(Text, Aeson.Object -> AesonTypes.Parser r)] ->
    Aeson.Object -> AesonTypes.Parser r
decodeVariantObj msg [] _ = "parseVariantObj of " <> msg <> " failed!" & fail
decodeVariantObj msg ((field, parser):rest) obj
    | Lens.has (Lens.ix field) obj = parser obj
    | otherwise = decodeVariantObj msg rest obj

decodeVariant ::
    String ->
    [(Text, Aeson.Object -> AesonTypes.Parser r)] ->
    Aeson.Value -> AesonTypes.Parser r
decodeVariant msg options (Aeson.Object obj) =
    decodeVariantObj msg options obj
decodeVariant msg _ _ = "parseVariant of " <> msg <> " expected object!" & fail

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

instance ToJSON (Pure # T.Type) where
    toJSON t =
        case t ^. _Pure of
        T.TFun (FuncType a b) -> "funcParam" ==> toJSON a <> "funcResult" ==> toJSON b
        T.TRecord composite   -> "record" ==> toJSON composite
        T.TVariant composite  -> "variant" ==> toJSON composite
        T.TVar (T.Var name)   -> "typeVar" ==> toJSON name
        T.TInst (NominalInst tId params) ->
            "nomId" ==> toJSON (T.nomId tId) <>
            encodeSquash "nomTypeArgs" (encodeIdentMap T.tvName) (params ^. T.tType . _QVarInstances) <>
            encodeSquash "nomRowArgs" (encodeIdentMap T.tvName) (params ^. T.tRow . _QVarInstances)
        & Aeson.Object

instance FromJSON (Pure # T.Type) where
    parseJSON json =
        Aeson.withObject "Type" ?? json $ \o ->
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
                <$> (decodeSquashed "nomTypeArgs" (decodeIdentMap T.Var parseJSON) o <&> QVarInstances)
                <*> (decodeSquashed "nomRowArgs" (decodeIdentMap T.Var parseJSON) o <&> QVarInstances)
                )
            <&> T.TInst
        ]
        <&> (_Pure #)

instance ToJSON T.RConstraints where
    toJSON (T.RowConstraints forbidden scopeLevel)
        | scopeLevel == mempty =
            Set.toList forbidden
            <&> T.tagName
            & toJSON
        | otherwise =
            -- We only encode top-level types, no skolem escape considerations...
            error "toJSON does not support inner-scoped types"

decodeCompositeConstraints ::
    [Aeson.Value] -> AesonTypes.Parser T.RConstraints
decodeCompositeConstraints json =
    traverse parseJSON json <&> map T.Tag <&> Set.fromList
    <&> (`T.RowConstraints` mempty)

encodeTypeVars :: T.Types # QVars -> Aeson.Object
encodeTypeVars (T.Types (QVars tvs) (QVars rvs)) =
    encodeSquash "typeVars"
    (toJSON . map (toJSON . T.tvName) . Set.toList)
    (Map.keysSet tvs)
    <>
    encodeSquash "rowVars"
    (encodeIdentMap T.tvName)
    rvs

decodeTypeVars :: Aeson.Object -> AesonTypes.Parser (T.Types # QVars)
decodeTypeVars obj =
    T.Types
    <$> ( decodeSquashed "typeVars"
        ( \tvs ->
            parseJSON tvs
            >>= traverse parseJSON
            <&> map (\name -> (T.Var name, mempty))
            <&> Map.fromList
        ) obj
        <&> QVars
        )
    <*> (decodeSquashed "rowVars" (decodeIdentMap T.Var decodeCompositeConstraints) obj
        <&> QVars)

instance ToJSON (Pure # T.Scheme) where
    toJSON (Pure (Scheme tvs typ)) =
        "schemeType" ==> toJSON typ <> encodeTypeVars tvs
        & Aeson.Object

instance FromJSON (Pure # T.Scheme) where
    parseJSON =
        Aeson.withObject "scheme" $ \obj ->
        do
            tvs <- decodeTypeVars obj
            typ <- obj .: "schemeType" >>= parseJSON
            _Pure # Scheme tvs typ & pure

encodeLeaf :: V.Leaf -> Aeson.Object
encodeLeaf =
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

decodeLeaf :: Aeson.Object -> AesonTypes.Parser V.Leaf
decodeLeaf =
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
                Aeson.Object x | HashMap.null x -> pure v
                x -> fail ("bad val for leaf " ++ show x)
            )

class Codec h where
    decodeBody :: Aeson.Object -> AesonTypes.Parser (h # Ann (Const UUID))
    encodeBody :: h # Ann (Const UUID) -> Aeson.Object

instance Codec h => ToJSON (Ann (Const UUID) # h) where
    toJSON (Ann uuid body) =
        encodeBody body
        & insertField "id" uuid
        & Aeson.Object

instance Codec h => FromJSON (Ann (Const UUID) # h) where
    parseJSON =
        Aeson.withObject "val" $ \obj ->
        Ann
        <$> (obj .: "id")
        <*> decodeBody obj

instance Codec V.Term where
    encodeBody body =
        case encBody of
        V.BApp (V.App func arg) ->
            "applyFunc" ==> c func <>
            "applyArg" ==> c arg
        V.BLam (V.TypedLam (V.Var varId) paramType res) ->
            "lamVar" ==> toJSON varId <>
            "lamParamType" ==> c paramType <>
            "lamBody" ==> c res
        V.BGetField (V.GetField reco tag) ->
            "getFieldRec" ==> c reco <>
            "getFieldName" ==> toJSON tag
        V.BRecExtend (RowExtend tag x rest) ->
            "extendTag" ==> toJSON tag <>
            "extendVal" ==> c x <>
            "extendRest" ==> c rest
        V.BInject (V.Inject tag x) ->
            "injectTag" ==> toJSON tag <>
            "injectVal" ==> c x
        V.BCase (RowExtend tag handler restHandler) ->
            "caseTag" ==> toJSON tag <>
            "caseHandler" ==> c handler <>
            "caseRest" ==> c restHandler
        V.BToNom (ToNom (T.NominalId nomId) x) ->
            "toNomId" ==> toJSON nomId <>
            "toNomVal" ==> c x
        V.BLeaf x -> encodeLeaf x
        where
            encBody :: V.Term # Const Aeson.Value
            encBody = hmap (Proxy @Codec #> Lens.Const . toJSON) body
            c x = x ^. Lens._Wrapped
    decodeBody obj =
        jsum
        [ V.App
        <$> (obj .: "applyFunc" <&> c)
        <*> (obj .: "applyArg" <&> c)
        <&> V.BApp
        , V.TypedLam
        <$> (obj .: "lamVar" >>= parseJSON <&> V.Var)
        <*> (obj .: "lamParamType" <&> c)
        <*> (obj .: "lamBody" <&> c)
        <&> V.BLam
        , V.GetField
        <$> (obj .: "getFieldRec" <&> c)
        <*> (obj .: "getFieldName" >>= parseJSON)
        <&> V.BGetField
        , RowExtend
        <$> (obj .: "extendTag" >>= parseJSON)
        <*> (obj .: "extendVal" <&> c)
        <*> (obj .: "extendRest" <&> c)
        <&> V.BRecExtend
        , V.Inject
        <$> (obj .: "injectTag" >>= parseJSON)
        <*> (obj .: "injectVal" <&> c)
        <&> V.BInject
        , RowExtend
        <$> (obj .: "caseTag" >>= parseJSON)
        <*> (obj .: "caseHandler" <&> c)
        <*> (obj .: "caseRest" <&> c)
        <&> V.BCase
        , ToNom
        <$> (obj .: "toNomId" >>= parseJSON <&> T.NominalId)
        <*> (obj .: "toNomVal" <&> c)
        <&> V.BToNom
        , decodeLeaf obj <&> V.BLeaf
        ] >>=
        htraverse (Proxy @Codec #> parseJSON . (^. Lens._Wrapped))
        where
            c :: Aeson.Value -> Const Aeson.Value # n
            c = Lens.Const

instance Codec (HCompose Prune T.Type) where
    decodeBody obj
        | (obj & Lens.at "id" .~ Nothing) == mempty =
            _HCompose # Pruned & pure
        | otherwise =
            obj .: "record"
            >>=
            \case
            Aeson.Array objs ->
                case traverse (^? _Object) objs >>= (^? Lens._Snoc) of
                Nothing -> fail "Malformed row fields"
                Just (fields, rTail) ->
                    foldr extend
                    (rTail .: "rowId" <&> Const <&> (`Ann` (hcomposed _Unpruned # T.REmpty)))
                    fields
                    where
                        extend field rest =
                            Ann
                            <$> (field .: "rowId" <&> Const)
                            <*> ( RowExtend
                                    <$> (field .: "rowTag" >>= parseJSON)
                                    <*> (field .: "id" <&> Const <&> (`Ann` (_HCompose # Pruned)) <&> (_HCompose #))
                                    <*> (rest <&> (_HCompose #))
                                    <&> (hcomposed _Unpruned . T._RExtend #)
                                )
            _ -> fail "Malformed params record"
            <&> (hcomposed _Unpruned . T._TRecord . _HCompose #)
    encodeBody (HCompose Pruned) = mempty
    encodeBody (HCompose (Unpruned (HCompose (T.TRecord (HCompose row))))) =
        "record" ==> array (go row)
        where
            go (Ann uuid (HCompose b)) =
                Aeson.Object (insertField "rowId" uuid x) : xs
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
                                & insertField "id" fId
                            , go r
                            )
                        Unpruned _ -> error "TODO"
    encodeBody (HCompose (Unpruned _)) = error "TODO"

encodeDefExpr :: Definition.Expr (Val UUID) -> Aeson.Object
encodeDefExpr (Definition.Expr x frozenDeps) =
    "val" ==> toJSON x <>
    encodeSquash "frozenDeps" id encodedDeps
    where
        encodedDeps =
            encodeSquash "defTypes"
                (encodeIdentMap V.vvName)
                (frozenDeps ^. depsGlobalTypes) <>
            encodeSquash "nominals"
                (encodeIdentMap T.nomId)
                (frozenDeps ^. depsNominals)

encodeDefBody :: Definition.Body (Val UUID) -> Aeson.Object
encodeDefBody (Definition.BodyBuiltin name) = "builtin" ==> toJSON name
encodeDefBody (Definition.BodyExpr defExpr) = encodeDefExpr defExpr

decodeDefExpr :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeDefExpr obj =
    Definition.Expr
    <$> (obj .: "val" >>= parseJSON)
    <*> decodeSquashed "frozenDeps" (Aeson.withObject "deps" decodeDeps) obj
    where
        decodeDeps o =
            Deps
            <$> decodeSquashed "defTypes" (decodeIdentMap V.Var parseJSON) o
            <*> decodeSquashed "nominals"
                (decodeIdentMap T.NominalId decodeNominal) o

decodeDefBody :: Aeson.Object -> AesonTypes.Parser (Definition.Body (Val UUID))
decodeDefBody obj =
    jsum
    [ obj .: "builtin" >>= parseJSON <&> Definition.BodyBuiltin
    , decodeDefExpr obj <&> Definition.BodyExpr
    ]

encodeRepl :: Encoder (Definition.Expr (Val UUID))
encodeRepl defExpr = "repl" ==> Aeson.Object (encodeDefExpr defExpr) & Aeson.Object

decodeRepl :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeRepl obj =
    obj .: "repl" >>= Aeson.withObject "defExpr" decodeDefExpr

insertField :: ToJSON a => Text -> a -> Aeson.Object -> Aeson.Object
insertField k v = HashMap.insert k (toJSON v)

instance ToJSON (Pure # NominalDecl T.Type) where toJSON = toJSON . encodeNominal

encodeNominal :: Pure # NominalDecl T.Type -> Aeson.Object
encodeNominal (Pure (NominalDecl params nominalType)) =
    "nomType" ==> toJSON (_Pure # nominalType)
    <> encodeTypeVars params

decodeNominal :: Aeson.Object -> AesonTypes.Parser (Pure # NominalDecl T.Type)
decodeNominal obj =
    NominalDecl
    <$> decodeTypeVars obj
    <*> (obj .: "nomType" >>= parseJSON <&> (^. _Pure))
    <&> (_Pure #)

encodeTagged :: Text -> (a -> Aeson.Object) -> ((T.Tag, Identifier), a) -> Aeson.Object
encodeTagged idAttrName encoder ((tag, ident), x) =
    encoder x
    & insertField idAttrName (toJSON ident)
    & insertField "tag" (toJSON tag)

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

instance ToJSON (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var)) where
    toJSON (Definition body scheme (presentationMode, tag, V.Var globalId)) =
        encodeTagged "def" encodeDefBody ((tag, globalId), body)
        & insertField "typ" (toJSON scheme)
        & insertField "defPresentationMode" (toJSON presentationMode)
        & Aeson.Object

decodeDef ::
    Aeson.Object ->
    AesonTypes.Parser (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
decodeDef obj =
    do
        ((tag, globalId), body) <- decodeTagged "def" decodeDefBody obj
        presentationMode <- obj .: "defPresentationMode" >>= parseJSON
        scheme <- obj .: "typ" >>= parseJSON
        Definition body scheme (presentationMode, tag, V.Var globalId) & pure

encodeTagOrder :: TagOrder -> Aeson.Object
encodeTagOrder tagOrder = "tagOrder" ==> toJSON tagOrder

encodeSymbol :: Tag.Symbol -> Aeson.Object
encodeSymbol Tag.NoSymbol = mempty
encodeSymbol (Tag.UniversalSymbol x) = "op" ==> Aeson.String x
encodeSymbol (Tag.DirectionalSymbol (Tag.DirOp l r)) =
    "op" ==> array [Aeson.String l, Aeson.String r]

encodeNamedTag :: Encoder (T.Tag, Tag)
encodeNamedTag (T.Tag ident, Tag order op names) =
    (encodeTagOrder order
    & insertField "tag" (toJSON ident))
    <> encodeSquash "names" id names <> encodeSymbol op
    & Aeson.Object

decodeSymbol :: Maybe Aeson.Value -> AesonTypes.Parser Tag.Symbol
decodeSymbol Nothing = pure Tag.NoSymbol
decodeSymbol (Just (Aeson.String x)) = Tag.UniversalSymbol x & pure
decodeSymbol (Just (Aeson.Array x)) =
    case x ^.. traverse of
    [Aeson.String l, Aeson.String r] ->
        Tag.DirOp l r & Tag.DirectionalSymbol & pure
    _ -> fail ("unexpected op names:" <> show x)
decodeSymbol x = fail ("unexpected op name: " <> show x)

decodeNamedTag :: Aeson.Object -> AesonTypes.Parser (T.Tag, Tag)
decodeNamedTag obj =
    (,)
    <$> (obj .: "tag" >>= parseJSON <&> T.Tag)
    <*>
    ( Tag
        <$> (obj .: "tagOrder")
        <*> decodeSymbol (obj ^. Lens.at "op")
        <*> decodeSquashed "names" parseJSON obj
    )

encodeTaggedLamVar ::
    Encoder (T.Tag, V.Var)
encodeTaggedLamVar (tag, V.Var ident) =
    encodeTagged "lamVar" (const mempty) ((tag, ident), ()) & Aeson.Object

decodeTaggedLamVar ::
    Aeson.Object -> AesonTypes.Parser (T.Tag, V.Var)
decodeTaggedLamVar json =
    decodeTagged "lamVar" (\_ -> pure ()) json
    <&> \((tag, ident), ()) ->
    (tag, V.Var ident)

encodeTaggedNominal :: Encoder ((T.Tag, T.NominalId), Maybe (Pure # NominalDecl T.Type))
encodeTaggedNominal ((tag, T.NominalId nomId), mNom) =
    foldMap encodeNominal mNom
    & Lens.at "nom" ?~ toJSON nomId
    & Lens.at "tag" ?~ toJSON tag
    & Aeson.Object

decodeTaggedNominal :: Aeson.Object -> AesonTypes.Parser ((T.Tag, T.NominalId), Maybe (Pure # NominalDecl T.Type))
decodeTaggedNominal json =
    decodeTagged "nom" decodeMNom json <&> _1 . _2 %~ T.NominalId
    where
        decodeMNom x =
            jsum
            [ decodeNominal x <&> Just
            , pure Nothing
            ]

encodeSchemaVersion :: Encoder Int
encodeSchemaVersion ver = "schemaVersion" ==> toJSON ver & Aeson.Object

decodeSchemaVersion :: Aeson.Object -> AesonTypes.Parser Int
decodeSchemaVersion = (.: "schemaVersion")
