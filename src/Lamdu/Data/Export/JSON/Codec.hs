-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Lamdu.Data.Export.JSON.Codec
    ( TagOrder
    , Entity(..), _EntitySchemaVersion, _EntityRepl, _EntityDef, _EntityTag, _EntityNominal, _EntityLamVar
    ) where

import           AST (Ann(..), monoChildren, Tree)
import           AST.Term.Nominal (ToNom(..))
import           AST.Term.Row (RowExtend(..))
import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import qualified Data.Vector as Vector
import           Lamdu.Calc.Identifier (Identifier, identHex, identFromHex)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type, Row)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Constraints (Constraints(..))
import qualified Lamdu.Calc.Type.Constraints as Constraints
import           Lamdu.Calc.Type.FlatComposite (FlatComposite(..))
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Nominal (Nominal(..))
import           Lamdu.Calc.Type.Scheme (Scheme(..))
import           Lamdu.Calc.Type.Vars (TypeVars(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.Infer as Infer

import           Lamdu.Prelude hiding ((.=))

type Encoded = Aeson.Value

type Encoder a = a -> Encoded
type Decoder a = Encoded -> AesonTypes.Parser a

type TagOrder = Int

data Entity
    = EntitySchemaVersion Int
    | EntityRepl (Definition.Expr (Val UUID))
    | EntityDef (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
    | EntityTag TagOrder (Maybe Text) T.Tag
    | EntityNominal T.Tag T.NominalId (Maybe Nominal)
    | EntityLamVar (Maybe Meta.ParamList) T.Tag UUID V.Var
Lens.makePrisms ''Entity

instance AesonTypes.ToJSON Entity where
    toJSON (EntitySchemaVersion ver) = encodeSchemaVersion ver
    toJSON (EntityRepl x) = encodeRepl x
    toJSON (EntityDef def) = encodeDef def
    toJSON (EntityTag tagOrder mName tag) = encodeNamedTag (tagOrder, mName, tag)
    toJSON (EntityNominal tag nomId nom) = encodeTaggedNominal ((tag, nomId), nom)
    toJSON (EntityLamVar mParamList tag lamI var) = encodeTaggedLamVar (mParamList, tag, lamI, var)

instance AesonTypes.FromJSON Entity where
    parseJSON =
        decodeVariant "entity"
        [ ("repl" , fmap EntityRepl . decodeRepl)
        , ("def"  , fmap EntityDef  . decodeDef)
        , ("name" , fmap (uncurry3 EntityTag) . decodeNamedTag)
        , ("nom"  , fmap (\((tag, nomId), nom) -> EntityNominal tag nomId nom) . decodeTaggedNominal)
        , ("lamId", fmap (uncurry4 EntityLamVar) . decodeTaggedLamVar)
        , ("schemaVersion", fmap EntitySchemaVersion . decodeSchemaVersion)
        ]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x0, x1, x2) = f x0 x1 x2

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x0, x1, x2, x3) = f x0 x1 x2 x3

encodePresentationMode :: Encoder Meta.PresentationMode
encodePresentationMode Meta.Verbose = Aeson.String "Verbose"
encodePresentationMode (Meta.Object tag) = Aeson.object ["Object" .= encodeTagId tag]
encodePresentationMode (Meta.Infix l r) =
    Aeson.object ["Infix" .= Aeson.Array (Vector.fromList [encodeTagId l, encodeTagId r])]

decodePresentationMode :: Decoder Meta.PresentationMode
decodePresentationMode (Aeson.String "Verbose") = pure Meta.Verbose
decodePresentationMode x =
    decodeVariant "Type"
    [ ("Object", \o -> o .: "Object" >>= decodeTagId <&> Meta.Object)
    , ("Infix", \o -> o .: "Infix" >>= decodeInfix)
    ] x
    where
        decodeInfix =
            Aeson.withArray "array of Infix tags" $
            \arr -> case Vector.toList arr of
            [l, r] -> Meta.Infix <$>  decodeTagId l <*> decodeTagId r
            _ -> fail "Expecting two infix tags"

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
    Text -> (a -> j) -> a -> [AesonTypes.Pair]
encodeSquash name encode x
    | x == mempty = []
    | otherwise = [name .= encode x]

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
decodeVariant msg options (AesonTypes.Object obj) =
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

encodeFlatComposite :: Encoder FlatComposite
encodeFlatComposite (FlatComposite fields rest) =
    case rest of
    Nothing -> encodedFields fields
    Just (T.Var name) ->
        AesonTypes.toJSON [encodedFields fields, encodeIdent name]
    where
        encodedFields = encodeIdentMap T.tagName encodeType

jsum :: [AesonTypes.Parser a] -> AesonTypes.Parser a
jsum parsers =
    parsers <&> toEither
    <&> swapEither & sequence <&> unlines & swapEither
    & fromEither
    where
        swapEither (Left x) = Right x
        swapEither (Right x) = Left x

decodeFlatComposite :: Decoder FlatComposite
decodeFlatComposite json =
    jsum
    [ do
          (encodedFields, encodedIdent) <- Aeson.parseJSON json
          fields <- decodeFields encodedFields
          tv <- decodeIdent encodedIdent <&> T.Var
          FlatComposite fields (Just tv) & pure

    , Aeson.parseJSON json
      >>= decodeFields
      <&> (`FlatComposite` Nothing)
    ]
    where
        decodeFields = decodeIdentMap T.Tag decodeType

encodeComposite :: Encoder Row
encodeComposite = encodeFlatComposite . FlatComposite.fromComposite

decodeComposite :: Decoder Row
decodeComposite = fmap FlatComposite.toComposite . decodeFlatComposite

encodeType :: Encoder Type
encodeType (T.TFun a b) = Aeson.object ["funcParam" .= encodeType a, "funcResult" .= encodeType b]
encodeType (T.TRecord composite) = Aeson.object ["record" .= encodeComposite composite]
encodeType (T.TVariant composite) = Aeson.object ["variant" .= encodeComposite composite]
encodeType (T.TVar (T.Var name)) = Aeson.object ["typeVar" .= encodeIdent name]
encodeType (T.TInst tId params) =
    ("nomId" .= encodeIdent (T.nomId tId)) :
    encodeSquash "nomParams" (encodeIdentMap T.typeParamId encodeType) params
    & Aeson.object

decodeType :: Decoder Type
decodeType json =
    Aeson.withObject "Type" ?? json $ \o ->
    jsum
    [ T.TFun
        <$> (o .: "funcParam" >>= decodeType)
        <*> (o .: "funcResult" >>= decodeType)
    , o .: "record" >>= decodeComposite <&> T.TRecord
    , o .: "variant" >>= decodeComposite <&> T.TVariant
    , o .: "typeVar" >>= decodeIdent <&> T.Var <&> T.TVar
    , do
          nomId <- o .: "nomId" >>= decodeIdent <&> T.NominalId
          params <- decodeSquashed "nomParams" (decodeIdentMap T.ParamId decodeType) o
          T.TInst nomId params & pure
    ]

encodeCompositeVarConstraints :: Constraints.CompositeVar -> [Encoded]
encodeCompositeVarConstraints (Constraints.CompositeVar forbidden) =
    Set.toList forbidden
    <&> T.tagName
    <&> encodeIdent

decodeCompositeConstraints ::
    [Encoded] -> AesonTypes.Parser Constraints.CompositeVar
decodeCompositeConstraints json =
    traverse decodeIdent json <&> map T.Tag <&> Set.fromList
    <&> Constraints.CompositeVar

encodeTypeVars :: Encoder (TypeVars, Constraints)
encodeTypeVars (TypeVars tvs rvs, cs) =
    concat
    [ encodeTVs "typeVars" tvs
    , encodeTVs "rowVars" rvs
    , encodeSquash "constraints" Aeson.object
      (encodeConstraints "rowVars" cs)
    ] & Aeson.object
    where
        encodeConstraints name (Constraints constraints) =
            encodeSquash name
            (encodeIdentMap T.tvName encodeCompositeVarConstraints)
            constraints
        encodeTVs name =
            encodeSquash name
            (Aeson.toJSON . map (encodeIdent . T.tvName) . Set.toList)

decodeTypeVars :: Decoder (TypeVars, Constraints)
decodeTypeVars =
    Aeson.withObject "TypeVars" $ \obj ->
    do
        let getTVs name = decodeSquashed name decodeTVs obj
        tvs <-
            TypeVars
            <$> getTVs "typeVars"
            <*> getTVs "rowVars"
        decodedConstraints <-
            decodeSquashed "constraints"
            ( Aeson.withObject "constraints" $ \constraints ->
              decodeConstraints "rowVars" constraints <&> Constraints
            ) obj
        pure (tvs, decodedConstraints)
    where
        decodeConstraints name =
            decodeSquashed name (decodeIdentMap T.Var decodeCompositeConstraints)
        decodeTV = fmap T.Var . decodeIdent
        decodeTVs = fmap Set.fromList . traverse decodeTV

encodeScheme :: Encoder Scheme
encodeScheme (Scheme tvs constraints typ) =
    ("schemeType" .= encodeType typ) :
    encodeSquash "schemeBinders" encodeTypeVars (tvs, constraints)
    & Aeson.object

decodeScheme :: Decoder Scheme
decodeScheme =
    Aeson.withObject "scheme" $ \obj ->
    do
        (tvs, constraints) <- decodeSquashed "schemeBinders" decodeTypeVars obj
        typ <- obj .: "schemeType" >>= decodeType
        Scheme tvs constraints typ & pure

encodeLeaf :: V.Leaf -> AesonTypes.Object
encodeLeaf =
    \case
    V.LHole -> l "hole"
    V.LRecEmpty -> l "recEmpty"
    V.LAbsurd -> l "absurd"
    V.LVar (V.Var var) -> HashMap.fromList ["var" .= encodeIdent var]
    V.LLiteral (V.PrimVal (T.NominalId primId) primBytes) ->
        HashMap.fromList
        [ "primId" .= encodeIdent primId
        , "primBytes" .= BS.unpack (Hex.encode primBytes)
        ]
    V.LFromNom (T.NominalId nomId) ->
        HashMap.fromList ["fromNomId" .= encodeIdent nomId]
    where
        l x = HashMap.fromList [x .= Aeson.object []]

decodeLeaf :: AesonTypes.Object -> AesonTypes.Parser V.Leaf
decodeLeaf =
    decodeVariantObj "leaf"
    [ l "hole" V.LHole
    , l "recEmpty" V.LRecEmpty
    , l "absurd" V.LAbsurd
    , ("var", \o -> o .: "var" >>= decodeIdent <&> V.Var <&> V.LVar)
    , ("primId",
        \obj ->
        do
            primId <- obj .: "primId" >>= decodeIdent <&> T.NominalId
            bytesHex <- obj .: "primBytes"
            let (primBytes, remain) = Hex.decode (BS.pack bytesHex)
            BS.null remain & guard
            V.PrimVal primId primBytes & pure
        <&> V.LLiteral
      )
    , ("fromNomId", \o -> o .: "fromNomId" >>= decodeIdent <&> T.NominalId <&> V.LFromNom)
    ]
    where
        l key v =
            ( key
            , \obj ->
                obj .: key >>=
                \case
                AesonTypes.Object x | HashMap.null x -> pure v
                x -> fail ("bad val for leaf " ++ show x)
            )

encodeVal :: Encoder (Val UUID)
encodeVal (Ann uuid body) =
    encodeValBody body
    & insertField "id" uuid
    & AesonTypes.Object

decodeVal :: Decoder (Val UUID)
decodeVal =
    AesonTypes.withObject "val" $ \obj ->
    Ann
    <$> (obj .: "id")
    <*> decodeValBody obj

encodeValBody :: Tree V.Term (Ann UUID) -> AesonTypes.Object
encodeValBody body =
    case body & monoChildren %~ Lens.Const . encodeVal of
    V.BApp (V.Apply func arg) ->
        HashMap.fromList ["applyFunc" .= c func, "applyArg" .= c arg]
    V.BLam (V.Lam (V.Var varId) res) ->
        HashMap.fromList ["lamVar" .= encodeIdent varId, "lamBody" .= c res]
    V.BGetField (V.GetField reco tag) ->
        HashMap.fromList ["getFieldRec" .= c reco, "getFieldName" .= encodeTagId tag]
    V.BRecExtend (RowExtend tag x rest) ->
        HashMap.fromList
        ["extendTag" .= encodeTagId tag, "extendVal" .= c x, "extendRest" .= c rest]
    V.BInject (V.Inject tag x) ->
        HashMap.fromList ["injectTag" .= encodeTagId tag, "injectVal" .= c x]
    V.BCase (RowExtend tag handler restHandler) ->
        HashMap.fromList ["caseTag" .= encodeTagId tag, "caseHandler" .= c handler, "caseRest" .= c restHandler]
    V.BToNom (ToNom (T.NominalId nomId) x) ->
        HashMap.fromList ["toNomId" .= encodeIdent nomId, "toNomVal" .= c x]
    V.BLeaf x -> encodeLeaf x
    where
        c x = x ^. Lens._Wrapped

decodeValBody :: AesonTypes.Object -> AesonTypes.Parser (Tree V.Term (Ann UUID))
decodeValBody obj =
    jsum
    [ V.Apply
      <$> (obj .: "applyFunc" <&> c <&> Lens.Const)
      <*> (obj .: "applyArg" <&> c <&> Lens.Const)
      <&> V.BApp
    , V.Lam
      <$> (obj .: "lamVar" >>= decodeIdent <&> V.Var)
      <*> (obj .: "lamBody" <&> c <&> Lens.Const)
      <&> V.BLam
    , V.GetField
      <$> (obj .: "getFieldRec" <&> c <&> Lens.Const)
      <*> (obj .: "getFieldName" >>= decodeTagId)
      <&> V.BGetField
    , RowExtend
      <$> (obj .: "extendTag" >>= decodeTagId)
      <*> (obj .: "extendVal" <&> c <&> Lens.Const)
      <*> (obj .: "extendRest" <&> c <&> Lens.Const)
      <&> V.BRecExtend
    , V.Inject
      <$> (obj .: "injectTag" >>= decodeTagId)
      <*> (obj .: "injectVal" <&> c <&> Lens.Const)
      <&> V.BInject
    , RowExtend
      <$> (obj .: "caseTag" >>= decodeTagId)
      <*> (obj .: "caseHandler" <&> c <&> Lens.Const)
      <*> (obj .: "caseRest" <&> c <&> Lens.Const)
      <&> V.BCase
    , ToNom
      <$> (obj .: "toNomId" >>= decodeIdent <&> T.NominalId)
      <*> (obj .: "toNomVal" <&> c <&> Lens.Const)
      <&> V.BToNom
    , decodeLeaf obj <&> V.BLeaf
    ] >>= monoChildren (decodeVal . (^. Lens._Wrapped . Lens._Wrapped))
    where
        c = Lens.Const

encodeDefExpr :: Definition.Expr (Val UUID) -> Aeson.Object
encodeDefExpr (Definition.Expr x frozenDeps) =
    ( "val" .= encodeVal x
    ) :
    encodeSquash "frozenDeps" HashMap.fromList encodedDeps
    & HashMap.fromList
    where
        encodedDeps =
            encodeSquash "defTypes"
                (encodeIdentMap V.vvName encodeScheme)
                (frozenDeps ^. Infer.depsGlobalTypes) ++
            encodeSquash "nominals"
                (encodeIdentMap T.nomId encodeNominal)
                (frozenDeps ^. Infer.depsNominals)

encodeDefBody :: Definition.Body (Val UUID) -> Aeson.Object
encodeDefBody (Definition.BodyBuiltin name) = HashMap.fromList ["builtin" .= encodeFFIName name]
encodeDefBody (Definition.BodyExpr defExpr) = encodeDefExpr defExpr

decodeDefExpr :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeDefExpr obj =
    Definition.Expr
    <$> (obj .: "val" >>= decodeVal)
    <*> decodeSquashed "frozenDeps" (Aeson.withObject "deps" decodeDeps) obj
    where
        decodeDeps o =
            Infer.Deps
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
encodeRepl defExpr = Aeson.object [ "repl" .= encodeDefExpr defExpr ]

decodeRepl :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeRepl obj =
    obj .: "repl" >>= Aeson.withObject "defExpr" decodeDefExpr

insertField :: Aeson.ToJSON a => Text -> a -> Aeson.Object -> Aeson.Object
insertField k v = HashMap.insert k (Aeson.toJSON v)

encodeNominal :: Nominal -> Aeson.Object
encodeNominal (Nominal paramsMap nominalType) =
    "nomType" .= encodeScheme nominalType :
    encodeSquash "typeParams"
    (encodeIdentMap T.typeParamId (encodeIdent . T.tvName)) paramsMap
    & HashMap.fromList

decodeNominal :: Aeson.Object -> AesonTypes.Parser Nominal
decodeNominal obj =
    Nominal
    <$> decodeSquashed "typeParams" (decodeIdentMap T.ParamId (fmap T.Var . decodeIdent)) obj
    <*> (obj .: "nomType" >>= decodeScheme)

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
encodeTagOrder tagOrder = HashMap.fromList ["tagOrder" .= tagOrder]

encodeNamedTag :: Encoder (TagOrder, Maybe Text, T.Tag)
encodeNamedTag (tagOrder, mName, T.Tag ident) =
    encodeTagOrder tagOrder
    & insertField "tag" (encodeIdent ident)
    & maybe id (insertField "name") mName
    & Aeson.Object

decodeNamedTag :: Aeson.Object -> AesonTypes.Parser (TagOrder, Maybe Text, T.Tag)
decodeNamedTag obj =
    (,,)
    <$> (obj .: "tagOrder")
    <*> optional (obj .: "name")
    <*> (obj .: "tag" >>= decodeIdent <&> T.Tag)

encodeParamList :: Encoder Meta.ParamList
encodeParamList = Aeson.toJSON . map encodeTagId

decodeParamList :: Decoder Meta.ParamList
decodeParamList json = Aeson.parseJSON json >>= traverse decodeTagId

encodeMaybe :: Encoder a -> Encoder (Maybe a)
encodeMaybe encoder mVal = mVal <&> encoder & Aeson.toJSON

decodeMaybe :: Decoder a -> Decoder (Maybe a)
decodeMaybe decoder json = Aeson.parseJSON json >>= traverse decoder

encodeLam :: (UUID, Maybe Meta.ParamList) -> Aeson.Object
encodeLam (lamI, mParamList) =
    HashMap.fromList
    [ "lamId" .= lamI
    , "lamFieldParams" .= encodeMaybe encodeParamList mParamList
    ]

decodeLam :: Aeson.Object -> AesonTypes.Parser (UUID, Maybe Meta.ParamList)
decodeLam obj =
    (,)
    <$> (obj .: "lamId")
    <*> (obj .: "lamFieldParams" >>= decodeMaybe decodeParamList)

encodeTaggedLamVar ::
    Encoder (Maybe Meta.ParamList, T.Tag, UUID, V.Var)
encodeTaggedLamVar (mParamList, tag, lamI, V.Var ident) =
    encodeTagged "lamVar" encodeLam ((tag, ident), (lamI, mParamList)) & Aeson.Object

decodeTaggedLamVar ::
    Aeson.Object -> AesonTypes.Parser (Maybe Meta.ParamList, T.Tag, UUID, V.Var)
decodeTaggedLamVar json =
    decodeTagged "lamVar" decodeLam json
    <&> \((tag, ident), (lamI, mParamList)) ->
    (mParamList, tag, lamI, V.Var ident)

encodeTaggedNominal :: Encoder ((T.Tag, T.NominalId), Maybe Nominal)
encodeTaggedNominal ((tag, T.NominalId nomId), mNom) =
    foldMap encodeNominal mNom
    & insertField "nom" (encodeIdent nomId)
    & insertField "tag" (encodeTagId tag)
    & Aeson.Object

decodeTaggedNominal :: Aeson.Object -> AesonTypes.Parser ((T.Tag, T.NominalId), Maybe Nominal)
decodeTaggedNominal json =
    decodeTagged "nom" decodeMNom json <&> _1 . _2 %~ T.NominalId
    where
        decodeMNom x =
            jsum
            [ decodeNominal x <&> Just
            , pure Nothing
            ]

encodeSchemaVersion :: Encoder Int
encodeSchemaVersion ver =
    HashMap.fromList
    [ "schemaVersion" .= ver
    ] & Aeson.Object

decodeSchemaVersion :: Aeson.Object -> AesonTypes.Parser Int
decodeSchemaVersion = (.: "schemaVersion")
