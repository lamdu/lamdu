-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeApplications, FlexibleInstances #-}
module Lamdu.Data.Export.JSON.Codec
    ( TagOrder
    , Version(..)
    , Entity(..), _EntitySchemaVersion, _EntityRepl, _EntityDef, _EntityTag, _EntityNominal, _EntityLamVar
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import           Data.Aeson ((.:))
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

type Encoder a = a -> Aeson.Value
type Decoder a = Aeson.Value -> AesonTypes.Parser a

type TagOrder = Int

newtype Version = Version Int
    deriving stock (Eq, Ord, Show)

data Entity
    = EntitySchemaVersion Version
    | EntityRepl (Definition.Expr (Val UUID))
    | EntityDef (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
    | EntityTag T.Tag Tag
    | EntityNominal T.Tag T.NominalId (Maybe (Pure # NominalDecl T.Type))
    | EntityLamVar T.Tag V.Var
Lens.makePrisms ''Entity

instance Aeson.ToJSON Entity where
    toJSON (EntitySchemaVersion ver) = encodeSchemaVersion ver
    toJSON (EntityRepl x) = encodeRepl x
    toJSON (EntityDef def) = encodeDef def
    toJSON (EntityTag tid tdata) = encodeNamedTag (tid, tdata)
    toJSON (EntityNominal tag nomId nom) = encodeTaggedNominal ((tag, nomId), nom)
    toJSON (EntityLamVar tag var) = encodeTaggedLamVar (tag, var)

instance Aeson.FromJSON Entity where
    parseJSON =
        decodeVariant "entity"
        [ ("repl", fmap EntityRepl . decodeRepl)
        , ("def", fmap EntityDef  . decodeDef)
        , ("tagOrder", fmap (uncurry EntityTag) . decodeNamedTag)
        , ("nom", fmap (\((tag, nomId), nom) -> EntityNominal tag nomId nom) . decodeTaggedNominal)
        , ("lamVar", fmap (uncurry EntityLamVar) . decodeTaggedLamVar)
        , ("schemaVersion", fmap EntitySchemaVersion . decodeSchemaVersion)
        ]

array :: [Aeson.Value] -> Aeson.Value
array = Aeson.Array . Vector.fromList

encodePresentationMode :: Encoder Meta.PresentationMode
encodePresentationMode Meta.Verbose = Aeson.String "Verbose"
encodePresentationMode (Meta.Operator l r) =
    "Operator" ~~> array [encodeTagId l, encodeTagId r]
    & Aeson.Object

decodePresentationMode :: Decoder Meta.PresentationMode
decodePresentationMode (Aeson.String "Verbose") = pure Meta.Verbose
decodePresentationMode x =
    decodeVariant "Type"
    [ ("Operator", \o -> o .: "Operator" >>= decodeOperator)
    ] x
    where
        decodeOperator =
            Aeson.withArray "array of Operator tags" $
            \arr -> case Vector.toList arr of
            [l, r] -> Meta.Operator <$>  decodeTagId l <*> decodeTagId r
            _ -> fail "Expecting two operator tags"

encodeFFIName :: Encoder Definition.FFIName
encodeFFIName (Definition.FFIName modulePath name) = modulePath ++ [name] & Aeson.toJSON

decodeFFIName :: Decoder Definition.FFIName
decodeFFIName =
    Aeson.withArray "array of FFIName components" $
    \arr -> case Vector.toList arr of
    [] -> fail "Expecting at least one FFIName component"
    xs ->
        Definition.FFIName
        <$> traverse Aeson.parseJSON (init xs)
        <*> Aeson.parseJSON (last xs)

encodeIdent :: Encoder Identifier
encodeIdent = Aeson.toJSON . identHex

toEither :: AesonTypes.Parser a -> Either String a
toEither parser = AesonTypes.parseEither (\() -> parser) ()

fromEither :: Either String a -> AesonTypes.Parser a
fromEither = either fail pure

decodeIdent :: Decoder Identifier
decodeIdent json =
    Aeson.parseJSON json
    <&> identFromHex
    >>= fromEither

encodeIdentMap ::
    Aeson.ToJSON b => (k -> Identifier) -> (a -> b) -> Encoder (Map k a)
encodeIdentMap getIdent encode m =
    m
    & Map.map encode
    & Map.mapKeys (identHex . getIdent)
    & Aeson.toJSON

{-# ANN decodeIdentMap ("HLint: ignore Use ^@.." :: String) #-}
decodeIdentMap ::
    (Aeson.FromJSON j, Ord k) =>
    (Identifier -> k) -> (j -> AesonTypes.Parser a) -> Decoder (Map k a)
decodeIdentMap fromIdent decode json =
    Aeson.parseJSON json
    <&> Map.toList
    >>= Lens.traverse %%~ decodePair
    <&> Map.fromList
    where
        decodePair (k, v) =
            (,)
            <$> (identFromHex k & fromEither <&> fromIdent)
            <*> decode v

encodeSquash ::
    (Eq a, Monoid a, Aeson.ToJSON j) =>
    Text -> (a -> j) -> a -> Aeson.Object
encodeSquash name encode x
    | x == mempty = mempty
    | otherwise = name ~~> Aeson.toJSON (encode x)

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
    (Aeson.FromJSON j, Monoid a) =>
    Text -> (j -> AesonTypes.Parser a) -> Aeson.Object -> AesonTypes.Parser a
decodeSquashed name decode o
    | Lens.has (Lens.ix name) o = o .: name >>= decode
    | otherwise = pure mempty

encodeTagId :: Encoder T.Tag
encodeTagId tag
    | tag == Anchors.anonTag = Aeson.Null
    | otherwise = T.tagName tag & encodeIdent

decodeTagId :: Decoder T.Tag
decodeTagId Aeson.Null = pure Anchors.anonTag
decodeTagId json = decodeIdent json <&> T.Tag

jsum :: [AesonTypes.Parser a] -> AesonTypes.Parser a
jsum parsers =
    parsers <&> toEither
    <&> swapEither & sequence <&> unlines & swapEither
    & fromEither
    where
        swapEither (Left x) = Right x
        swapEither (Right x) = Left x

encodeComposite :: Encoder (Pure # T.Row)
encodeComposite  =
    array . go
    where
        go (Pure T.REmpty) = []
        go (Pure (T.RVar (T.Var name))) =
            ["rowVar" ~~> encodeIdent name & Aeson.Object]
        go (Pure (T.RExtend (RowExtend t v r))) =
            (encodeType v & _Object . Lens.at "rowTag" ?~ encodeTagId t) : go r

decodeComposite :: Decoder (Pure # T.Row)
decodeComposite (Aeson.Array vec) =
    case items ^? Lens._Snoc >>= _2 (^? _Object . Lens.ix "rowVar") of
    Just (elems, restVar) ->
        foldr field (decodeIdent restVar <&> Pure . T.RVar . T.Var) elems
    _ -> foldr field (Pure T.REmpty & pure) items
    where
        items = Vector.toList vec
        field x rest =
            Aeson.withObject "RowField" ?? x $
            \o ->
            RowExtend
            <$> (o .: "rowTag" >>= decodeTagId)
            <*> decodeType x
            <*> rest
            <&> Pure . T.RExtend
decodeComposite x = fail ("malformed row" <> show x)

encodeType :: Encoder (Pure # T.Type)
encodeType t =
    case t ^. _Pure of
    T.TFun (FuncType a b) -> "funcParam" ~~> encodeType a <> "funcResult" ~~> encodeType b
    T.TRecord composite   -> "record" ~~> encodeComposite composite
    T.TVariant composite  -> "variant" ~~> encodeComposite composite
    T.TVar (T.Var name)   -> "typeVar" ~~> encodeIdent name
    T.TInst (NominalInst tId params) ->
        "nomId" ~~> encodeIdent (T.nomId tId) <>
        encodeSquash "nomTypeArgs" (encodeIdentMap T.tvName encodeType) (params ^. T.tType . _QVarInstances) <>
        encodeSquash "nomRowArgs" (encodeIdentMap T.tvName encodeComposite) (params ^. T.tRow . _QVarInstances)
    & Aeson.Object

decodeType :: Decoder (Pure # T.Type)
decodeType json =
    Aeson.withObject "Type" ?? json $ \o ->
    jsum
    [ FuncType
        <$> (o .: "funcParam" >>= decodeType)
        <*> (o .: "funcResult" >>= decodeType)
        <&> T.TFun
    , o .: "record" >>= decodeComposite <&> T.TRecord
    , o .: "variant" >>= decodeComposite <&> T.TVariant
    , o .: "typeVar" >>= decodeIdent <&> T.Var <&> T.TVar
    , NominalInst
        <$> (o .: "nomId" >>= decodeIdent <&> T.NominalId)
        <*> (T.Types
            <$> (decodeSquashed "nomTypeArgs" (decodeIdentMap T.Var decodeType) o <&> QVarInstances)
            <*> (decodeSquashed "nomRowArgs" (decodeIdentMap T.Var decodeComposite) o <&> QVarInstances)
            )
        <&> T.TInst
    ]
    <&> (_Pure #)

encodeCompositeVarConstraints :: T.RConstraints -> [Aeson.Value]
encodeCompositeVarConstraints (T.RowConstraints forbidden scopeLevel)
    | scopeLevel == mempty =
        forbidden ^.. Lens.folded
        <&> T.tagName
        <&> encodeIdent
    | otherwise =
        -- We only encode top-level types, no skolem escape considerations...
        error "encodeCompositeVarConstraints does not support inner-scoped types"

decodeCompositeConstraints ::
    [Aeson.Value] -> AesonTypes.Parser T.RConstraints
decodeCompositeConstraints json =
    traverse decodeIdent json <&> map T.Tag <&> Set.fromList
    <&> (`T.RowConstraints` mempty)

encodeTypeVars :: T.Types # QVars -> Aeson.Object
encodeTypeVars (T.Types (QVars tvs) (QVars rvs)) =
    encodeSquash "typeVars"
    (Aeson.toJSON . map (encodeIdent . T.tvName))
    (tvs ^.. Lens.itraversed . Lens.asIndex)
    <>
    encodeSquash "rowVars"
    (encodeIdentMap T.tvName encodeCompositeVarConstraints)
    rvs

decodeTypeVars :: Aeson.Object -> AesonTypes.Parser (T.Types # QVars)
decodeTypeVars obj =
    T.Types
    <$> ( decodeSquashed "typeVars"
        ( \tvs ->
            Aeson.parseJSON tvs
            >>= traverse decodeIdent
            <&> map (\name -> (T.Var name, mempty))
            <&> Map.fromList
        ) obj
        <&> QVars
        )
    <*> (decodeSquashed "rowVars" (decodeIdentMap T.Var decodeCompositeConstraints) obj
        <&> QVars)

encodeScheme :: Encoder (Pure # T.Scheme)
encodeScheme (Pure (Scheme tvs typ)) =
    "schemeType" ~~> encodeType typ <> encodeTypeVars tvs
    & Aeson.Object

decodeScheme :: Decoder (Pure # T.Scheme)
decodeScheme =
    Aeson.withObject "scheme" $ \obj ->
    do
        tvs <- decodeTypeVars obj
        typ <- obj .: "schemeType" >>= decodeType
        _Pure # Scheme tvs typ & pure

encodeLeaf :: V.Leaf -> Aeson.Object
encodeLeaf =
    \case
    V.LHole -> l "hole"
    V.LRecEmpty -> l "recEmpty"
    V.LAbsurd -> l "absurd"
    V.LVar (V.Var var) -> "var" ~~> encodeIdent var
    V.LLiteral (V.PrimVal (T.NominalId primId) primBytes) ->
        "primId" ~~> encodeIdent primId <>
        "primBytes" ~~> Aeson.toJSON (BS.unpack (Hex.encode primBytes))
    V.LFromNom (T.NominalId nomId) ->
        "fromNomId" ~~> encodeIdent nomId
    V.LGetField tag -> "getField" ~~> encodeTagId tag
    V.LInject tag -> "inject" ~~> encodeTagId tag
    where
        l x = x ~~> Aeson.object []

decodeLeaf :: Aeson.Object -> AesonTypes.Parser V.Leaf
decodeLeaf =
    decodeVariantObj "leaf"
    [ l "hole" V.LHole
    , l "recEmpty" V.LRecEmpty
    , l "absurd" V.LAbsurd
    , f "var" (fmap (V.LVar . V.Var) . decodeIdent)
    , f "fromNomId" (fmap (V.LFromNom . T.NominalId) . decodeIdent)
    , f "getField" (fmap V.LGetField . decodeTagId)
    , f "inject" (fmap V.LInject . decodeTagId)
    , ("primId",
        \obj ->
        do
            primId <- obj .: "primId" >>= decodeIdent <&> T.NominalId
            bytesHex <- obj .: "primBytes"
            primBytes <- Hex.decode (BS.pack bytesHex) & fromEither
            V.PrimVal primId primBytes & pure
        <&> V.LLiteral
      )
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
        f key v = (key, \o -> o .: key >>= v)

encodeVal :: Codec h => Encoder (Ann (Const UUID) # h)
encodeVal (Ann uuid body) =
    encodeBody body
    & insertField "id" uuid
    & Aeson.Object

class Codec h where
    decodeBody :: Aeson.Object -> AesonTypes.Parser (h # Annotated UUID)
    encodeBody :: h # Annotated UUID -> Aeson.Object

decodeVal :: Codec h => Decoder (Ann (Const UUID) # h)
decodeVal =
    Aeson.withObject "val" $ \obj ->
    Ann
    <$> (obj .: "id")
    <*> decodeBody obj

instance Codec V.Term where
    encodeBody body =
        case encBody of
        V.BApp (V.App func arg) ->
            "applyFunc" ~~> c func <>
            "applyArg" ~~> c arg
        V.BLam (V.TypedLam (V.Var varId) paramType res) ->
            "lamVar" ~~> encodeIdent varId <>
            "lamParamType" ~~> c paramType <>
            "lamBody" ~~> c res
        V.BRecExtend (RowExtend tag x rest) ->
            "extendTag" ~~> encodeTagId tag <>
            "extendVal" ~~> c x <>
            "extendRest" ~~> c rest
        V.BCase (RowExtend tag handler restHandler) ->
            "caseTag" ~~> encodeTagId tag <>
            "caseHandler" ~~> c handler <>
            "caseRest" ~~> c restHandler
        V.BToNom (ToNom (T.NominalId nomId) x) ->
            "toNomId" ~~> encodeIdent nomId <>
            "toNomVal" ~~> c x
        V.BLeaf x -> encodeLeaf x
        where
            encBody :: V.Term # Const Aeson.Value
            encBody = hmap (Proxy @Codec #> Const . encodeVal) body
            c x = x ^. Lens._Wrapped
    decodeBody obj =
        jsum
        [ V.App
        <$> (obj .: "applyFunc" <&> c)
        <*> (obj .: "applyArg" <&> c)
        <&> V.BApp
        , V.TypedLam
        <$> (obj .: "lamVar" >>= decodeIdent <&> V.Var)
        <*> (obj .: "lamParamType" <&> c)
        <*> (obj .: "lamBody" <&> c)
        <&> V.BLam
        , RowExtend
        <$> (obj .: "extendTag" >>= decodeTagId)
        <*> (obj .: "extendVal" <&> c)
        <*> (obj .: "extendRest" <&> c)
        <&> V.BRecExtend
        , RowExtend
        <$> (obj .: "caseTag" >>= decodeTagId)
        <*> (obj .: "caseHandler" <&> c)
        <*> (obj .: "caseRest" <&> c)
        <&> V.BCase
        , ToNom
        <$> (obj .: "toNomId" >>= decodeIdent <&> T.NominalId)
        <*> (obj .: "toNomVal" <&> c)
        <&> V.BToNom
        , decodeLeaf obj <&> V.BLeaf
        ] >>=
        htraverse (Proxy @Codec #> decodeVal . (^. Lens._Wrapped))
        where
            c :: Aeson.Value -> Const Aeson.Value # n
            c = Const

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
                                    <$> (field .: "rowTag" >>= decodeTagId)
                                    <*> (field .: "id" <&> Const <&> (`Ann` (_HCompose # Pruned)) <&> (_HCompose #))
                                    <*> (rest <&> (_HCompose #))
                                    <&> (hcomposed _Unpruned . T._RExtend #)
                                )
            _ -> fail "Malformed params record"
            <&> (hcomposed _Unpruned . T._TRecord . _HCompose #)
    encodeBody (HCompose Pruned) = mempty
    encodeBody (HCompose (Unpruned (HCompose (T.TRecord (HCompose row))))) =
        "record" ~~> array (go row)
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
                            ( "rowTag" ~~> encodeTagId t
                                & insertField "id" fId
                            , go r
                            )
                        Unpruned _ -> error "TODO"
    encodeBody (HCompose (Unpruned _)) = error "TODO"

encodeDefExpr :: Definition.Expr (Val UUID) -> Aeson.Object
encodeDefExpr (Definition.Expr x frozenDeps) =
    "val" ~~> encodeVal x <>
    encodeSquash "frozenDeps" id encodedDeps
    where
        encodedDeps =
            encodeSquash "defTypes"
                (encodeIdentMap V.vvName encodeScheme)
                (frozenDeps ^. depsGlobalTypes) <>
            encodeSquash "nominals"
                (encodeIdentMap T.nomId encodeNominal)
                (frozenDeps ^. depsNominals)

encodeDefBody :: Definition.Body (Val UUID) -> Aeson.Object
encodeDefBody (Definition.BodyBuiltin name) = "builtin" ~~> encodeFFIName name
encodeDefBody (Definition.BodyExpr defExpr) = encodeDefExpr defExpr

decodeDefExpr :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeDefExpr obj =
    Definition.Expr
    <$> (obj .: "val" >>= decodeVal)
    <*> decodeSquashed "frozenDeps" (Aeson.withObject "deps" decodeDeps) obj
    where
        decodeDeps o =
            Deps
            <$> decodeSquashed "defTypes" (decodeIdentMap V.Var decodeScheme) o
            <*> decodeSquashed "nominals"
                (decodeIdentMap T.NominalId decodeNominal) o

decodeDefBody :: Aeson.Object -> AesonTypes.Parser (Definition.Body (Val UUID))
decodeDefBody obj =
    jsum
    [ obj .: "builtin" >>= decodeFFIName <&> Definition.BodyBuiltin
    , decodeDefExpr obj <&> Definition.BodyExpr
    ]

encodeRepl :: Encoder (Definition.Expr (Val UUID))
encodeRepl defExpr = "repl" ~~> Aeson.Object (encodeDefExpr defExpr) & Aeson.Object

decodeRepl :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeRepl obj =
    obj .: "repl" >>= Aeson.withObject "defExpr" decodeDefExpr

insertField :: Aeson.ToJSON a => Text -> a -> Aeson.Object -> Aeson.Object
insertField k v = HashMap.insert k (Aeson.toJSON v)

encodeNominal :: Pure # NominalDecl T.Type -> Aeson.Object
encodeNominal (Pure (NominalDecl params nominalType)) =
    "nomType" ~~> encodeScheme (_Pure # nominalType)
    <> encodeTypeVars params

decodeNominal :: Aeson.Object -> AesonTypes.Parser (Pure # NominalDecl T.Type)
decodeNominal obj =
    NominalDecl
    <$> decodeTypeVars obj
    <*> (obj .: "nomType" >>= decodeScheme <&> (^. _Pure))
    <&> (_Pure #)

encodeTagged :: Text -> (a -> Aeson.Object) -> ((T.Tag, Identifier), a) -> Aeson.Object
encodeTagged idAttrName encoder ((tag, ident), x) =
    encoder x
    & insertField idAttrName (encodeIdent ident)
    & insertField "tag" (encodeTagId tag)

decodeTagged ::
    Text ->
    (Aeson.Object -> AesonTypes.Parser a) ->
    Aeson.Object -> AesonTypes.Parser ((T.Tag, Identifier), a)
decodeTagged idAttrName decoder obj =
    (,)
    <$> ( (,)
          <$> (obj .: "tag" >>= decodeTagId)
          <*> (obj .: idAttrName >>= decodeIdent)
        )
    <*> decoder obj

encodeDef ::
    Encoder (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
encodeDef (Definition body scheme (presentationMode, tag, V.Var globalId)) =
    encodeTagged "def" encodeDefBody ((tag, globalId), body)
    & insertField "typ" (encodeScheme scheme)
    & insertField "defPresentationMode" (encodePresentationMode presentationMode)
    & Aeson.Object

decodeDef ::
    Aeson.Object ->
    AesonTypes.Parser (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
decodeDef obj =
    do
        ((tag, globalId), body) <- decodeTagged "def" decodeDefBody obj
        presentationMode <-
            obj .: "defPresentationMode" >>= decodePresentationMode
        scheme <- obj .: "typ" >>= decodeScheme
        Definition body scheme (presentationMode, tag, V.Var globalId) & pure

encodeTagOrder :: TagOrder -> Aeson.Object
encodeTagOrder tagOrder = "tagOrder" ~~> Aeson.toJSON tagOrder

encodeSymbol :: Tag.Symbol -> Aeson.Object
encodeSymbol Tag.NoSymbol = mempty
encodeSymbol (Tag.UniversalSymbol x) = "op" ~~> Aeson.String x
encodeSymbol (Tag.DirectionalSymbol (Tag.DirOp l r)) =
    "op" ~~> array [Aeson.String l, Aeson.String r]

encodeNamedTag :: Encoder (T.Tag, Tag)
encodeNamedTag (T.Tag ident, Tag order op names) =
    (encodeTagOrder order
    & insertField "tag" (encodeIdent ident))
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
    <$> (obj .: "tag" >>= decodeIdent <&> T.Tag)
    <*>
    ( Tag
        <$> (obj .: "tagOrder")
        <*> decodeSymbol (obj ^. Lens.at "op")
        <*> decodeSquashed "names" Aeson.parseJSON obj
    )

encodeTaggedLamVar ::
    Encoder (T.Tag, V.Var)
encodeTaggedLamVar (tag, V.Var ident) =
    encodeTagged "lamVar" mempty ((tag, ident), ()) & Aeson.Object

decodeTaggedLamVar ::
    Aeson.Object -> AesonTypes.Parser (T.Tag, V.Var)
decodeTaggedLamVar json =
    decodeTagged "lamVar" (\_ -> pure ()) json
    <&> \((tag, ident), ()) ->
    (tag, V.Var ident)

encodeTaggedNominal :: Encoder ((T.Tag, T.NominalId), Maybe (Pure # NominalDecl T.Type))
encodeTaggedNominal ((tag, T.NominalId nomId), mNom) =
    foldMap encodeNominal mNom
    & Lens.at "nom" ?~ encodeIdent nomId
    & Lens.at "tag" ?~ encodeTagId tag
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

encodeSchemaVersion :: Encoder Version
encodeSchemaVersion (Version ver) =
    "schemaVersion" ~~> Aeson.toJSON ver & Aeson.Object

decodeSchemaVersion :: Aeson.Object -> AesonTypes.Parser Version
decodeSchemaVersion = (.: "schemaVersion") <&> fmap Version
