-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE NoImplicitPrelude, LambdaCase, OverloadedStrings, PatternGuards #-}
module Lamdu.Data.Export.JSON.Codec
    ( Encoder
      , encodeRepl, encodeDef, encodeArray
      , encodeNamedTag
      , encodeNamedNominal
      , encodeNamedLamVar
    , Decoder
      , decodeRepl, decodeDef, decodeArray
      , decodeNamedTag
      , decodeNamedNominal
      , decodeNamedLamVar
    , Encoded, runDecoder
    ) where

import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Control.Lens.Operators hiding ((.=))
import           Control.Lens.Tuple
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import           Data.ByteString.Hex (showHexBytes, parseHexBytes)
import           Data.Either.Combinators (swapEither)
import qualified Data.HashMap.Strict as HashMap
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Data.UUID.Aeson ()
import           Data.UUID.Types (UUID)
import qualified Data.Vector as Vector
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Constraints (Constraints(..), CompositeVarConstraints(..))
import           Lamdu.Expr.FlatComposite (FlatComposite(..))
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import           Lamdu.Expr.Identifier (Identifier, identHex, identFromHex)
import           Lamdu.Expr.Nominal (Nominal(..))
import           Lamdu.Expr.Scheme (Scheme(..))
import           Lamdu.Expr.Type (Type, Composite)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.TypeVars (TypeVars(..))
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

type Encoded = Aeson.Value

type Encoder a = a -> Encoded
type Decoder a = Encoded -> AesonTypes.Parser a

runDecoder :: Decoder a -> Encoded -> Either String a
runDecoder = AesonTypes.parseEither

encodeArray :: Encoder [Aeson.Value]
encodeArray = Aeson.Array . Vector.fromList

decodeArray :: Decoder [Aeson.Value]
decodeArray = fmap Vector.toList . Aeson.parseJSON

encodePresentationMode :: Encoder Anchors.PresentationMode
encodePresentationMode Anchors.OO = Aeson.String "OO"
encodePresentationMode Anchors.Verbose = Aeson.String "Verbose"
encodePresentationMode (Anchors.Infix prec) = Aeson.toJSON (Aeson.String "Infix", prec)

decodePresentationMode :: Decoder Anchors.PresentationMode
decodePresentationMode (Aeson.String "OO") = pure Anchors.OO
decodePresentationMode (Aeson.String "Verbose") = pure Anchors.Verbose
decodePresentationMode (Aeson.Array arr)
    | [Aeson.String "Infix", Aeson.Number prec] <- Vector.toList arr =
          pure (Anchors.Infix (truncate prec))
decodePresentationMode other = fail $ "Unexpected presentation mode: " ++ show other

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
fromEither = either fail return

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
    Aeson.ToJSON j => (a -> Bool) -> String -> (a -> j) -> a -> [AesonTypes.Pair]
encodeSquash isEmpty name encode val
    | isEmpty val = []
    | otherwise = [fromString name .= encode val]

decodeSquashed ::
    (Aeson.FromJSON j, Monoid a) =>
    String -> (j -> AesonTypes.Parser a) -> Aeson.Object -> AesonTypes.Parser a
decodeSquashed name decode o =
    jsum
    [ o .: fromString name >>= decode
    , pure mempty
    ]

encodeTagId :: Encoder T.Tag
encodeTagId (T.Tag ident) = encodeIdent ident

decodeTagId :: Decoder T.Tag
decodeTagId json = decodeIdent json <&> T.Tag

encodeFlatComposite :: Encoder (FlatComposite p)
encodeFlatComposite = \case
    FlatComposite fields Nothing -> encodedFields fields
    FlatComposite fields (Just (T.Var name)) ->
        encodeArray [encodedFields fields, encodeIdent name]
    where
        encodedFields = encodeIdentMap T.tagName encodeType

jsum :: [AesonTypes.Parser a] -> AesonTypes.Parser a
jsum parsers =
    parsers <&> toEither
    <&> swapEither & sequence <&> unlines & swapEither
    & fromEither

decodeFlatComposite :: Decoder (FlatComposite p)
decodeFlatComposite json =
    jsum
    [ do
          (encodedFields, encodedIdent) <- Aeson.parseJSON json
          fields <- decodeFields encodedFields
          tv <- decodeIdent encodedIdent <&> T.Var
          FlatComposite fields (Just tv) & return

    , Aeson.parseJSON json
      >>= decodeFields
      <&> (`FlatComposite` Nothing)
    ]
    where
        decodeFields = decodeIdentMap T.Tag decodeType

encodeComposite :: Encoder (Composite p)
encodeComposite = encodeFlatComposite . FlatComposite.fromComposite

decodeComposite :: Decoder (Composite p)
decodeComposite = fmap FlatComposite.toComposite . decodeFlatComposite

encodeType :: Encoder Type
encodeType (T.TFun a b) = Aeson.object ["funcParam" .= encodeType a, "funcResult" .= encodeType b]
encodeType (T.TRecord composite) = Aeson.object ["record" .= encodeComposite composite]
encodeType (T.TSum composite) = Aeson.object ["sum" .= encodeComposite composite]
encodeType (T.TVar (T.Var name)) = Aeson.object ["typeVar" .= encodeIdent name]
encodeType (T.TInst tId params) =
    ("nomId" .= encodeIdent (T.nomId tId)) :
    encodeSquash null "nomParams" (encodeIdentMap T.typeParamId encodeType) params
    & Aeson.object

decodeType :: Decoder Type
decodeType json =
    Aeson.withObject "Type" ?? json $ \o ->
    jsum
    [ T.TFun
        <$> (o .: "funcParam" >>= decodeType)
        <*> (o .: "funcResult" >>= decodeType)
    , o .: "record" >>= decodeComposite <&> T.TRecord
    , o .: "sum" >>= decodeComposite <&> T.TSum
    , o .: "typeVar" >>= decodeIdent <&> T.Var <&> T.TVar
    , do
          nomId <- o .: "nomId" >>= decodeIdent <&> T.NominalId
          params <- decodeSquashed "nomParams" (decodeIdentMap T.ParamId decodeType) o
          T.TInst nomId params & pure
    ]

encodeTypeVars :: Encoder (TypeVars, Constraints)
encodeTypeVars (TypeVars tvs rtvs stvs, Constraints productConstraints sumConstraints) =
    concat
    [ encodeTVs "typeVars" tvs
    , encodeTVs "recordTypeVars" rtvs
    , encodeTVs "sumTypeVars" stvs
    , encodeSquash null "constraints" Aeson.object
      (encodeConstraints "recordTypeVars" productConstraints ++
       encodeConstraints "sumTypeVars" sumConstraints)
    ] & Aeson.object
    where
        encodeConstraints name (CompositeVarConstraints constraints) =
            encodeSquash Map.null name
            (encodeIdentMap T.tvName (map (encodeIdent . T.tagName) . Set.toList))
            constraints
        encodeTVs name =
            encodeSquash Set.null name
            (Aeson.toJSON . map (encodeIdent . T.tvName) . Set.toList)

decodeTypeVars :: Decoder (TypeVars, Constraints)
decodeTypeVars =
    Aeson.withObject "TypeVars" $ \obj ->
    do
        let getTVs name = decodeSquashed name decodeTVs obj
        tvs <-
            TypeVars
            <$> getTVs "typeVars"
            <*> getTVs "recordTypeVars"
            <*> getTVs "sumTypeVars"
        decodedConstraints <-
            decodeSquashed "constraints"
            ( Aeson.withObject "constraints" $ \constraints ->
              let  getCs name = decodeConstraints name constraints <&> CompositeVarConstraints
              in   Constraints <$> getCs "recordTypeVars" <*> getCs "sumTypeVars"
            ) obj
        return (tvs, decodedConstraints)
    where
        decodeForbiddenFields json =
            traverse decodeIdent json <&> map T.Tag <&> Set.fromList
        decodeConstraints name =
            decodeSquashed name (decodeIdentMap T.Var decodeForbiddenFields)
        decodeTV = fmap T.Var . decodeIdent
        decodeTVs = fmap Set.fromList . traverse decodeTV

encodeScheme :: Encoder Scheme
encodeScheme (Scheme tvs constraints typ) =
    ("schemeType" .= encodeType typ) :
    encodeSquash (== mempty) "schemeBinders" encodeTypeVars (tvs, constraints)
    & Aeson.object

decodeScheme :: Decoder Scheme
decodeScheme =
    Aeson.withObject "scheme" $ \obj ->
    do
        (tvs, constraints) <- decodeSquashed "schemeBinders" decodeTypeVars obj
        typ <- obj .: "schemeType" >>= decodeType
        Scheme tvs constraints typ & return

encodeLeaf :: V.Leaf -> AesonTypes.Object
encodeLeaf =
    \case
    V.LHole -> leaf "hole"
    V.LRecEmpty -> leaf "recEmpty"
    V.LAbsurd -> leaf "absurd"
    V.LVar (V.Var var) -> HashMap.fromList ["var" .= encodeIdent var]
    V.LLiteral (V.PrimVal (T.NominalId primId) primBytes) ->
        HashMap.fromList
        [ "primId" .= encodeIdent primId
        , "primBytes" .= showHexBytes primBytes
        ]
    where
        leaf x = HashMap.fromList [x .= Aeson.object []]

decodeLeaf :: AesonTypes.Object -> AesonTypes.Parser V.Leaf
decodeLeaf obj =
    jsum
    [ leaf "hole" V.LHole
    , leaf "recEmpty" V.LRecEmpty
    , leaf "absurd" V.LAbsurd
    , obj .: "var" >>= decodeIdent <&> V.Var <&> V.LVar
    , do
          primId <- obj .: "primId" >>= decodeIdent <&> T.NominalId
          primBytes <- obj .: "primBytes" >>= fromEither . parseHexBytes
          V.PrimVal primId primBytes & pure
      <&> V.LLiteral
    ]
    where
        leaf key val =
            obj .: key >>=
            \case
            AesonTypes.Object x | HashMap.null x -> return val
            x -> fail ("bad val for leaf " ++ show x)

encodeVal :: Encoder (Val UUID)
encodeVal (Val uuid body) =
    encodeValBody body
    & insertField "id" uuid
    & AesonTypes.Object

decodeVal :: Decoder (Val UUID)
decodeVal =
    AesonTypes.withObject "val" $ \obj ->
    Val
    <$> (obj .: "id")
    <*> decodeValBody obj

encodeValBody :: V.Body (Val UUID) -> AesonTypes.Object
encodeValBody body =
    case body <&> encodeVal of
    V.BApp (V.Apply func arg) ->
        HashMap.fromList ["applyFunc" .= func, "applyArg" .= arg]
    V.BAbs (V.Lam (V.Var varId) res) ->
        HashMap.fromList ["lamVar" .= encodeIdent varId, "lamBody" .= res]
    V.BGetField (V.GetField reco tag) ->
        HashMap.fromList ["getFieldRec" .= reco, "getFieldName" .= encodeTagId tag]
    V.BRecExtend (V.RecExtend tag val rest) ->
        HashMap.fromList
        ["extendTag" .= encodeTagId tag, "extendVal" .= val, "extendRest" .= rest]
    V.BInject (V.Inject tag val) ->
        HashMap.fromList ["injectTag" .= encodeTagId tag, "injectVal" .= val]
    V.BCase (V.Case tag handler restHandler) ->
        HashMap.fromList ["caseTag" .= encodeTagId tag, "caseHandler" .= handler, "caseRest" .= restHandler]
    V.BToNom (V.Nom (T.NominalId nomId) val) ->
        HashMap.fromList ["toNomId" .= encodeIdent nomId, "toNomVal" .= val]
    V.BFromNom (V.Nom (T.NominalId nomId) val) ->
        HashMap.fromList ["fromNomId" .= encodeIdent nomId, "fromNomVal" .= val]
    V.BLeaf leaf -> encodeLeaf leaf

decodeValBody :: AesonTypes.Object -> AesonTypes.Parser (V.Body (Val UUID))
decodeValBody obj =
    jsum
    [ V.Apply <$> obj .: "applyFunc" <*> obj .: "applyArg" <&> V.BApp
    , V.Lam <$> (obj .: "lamVar" >>= decodeIdent <&> V.Var) <*> (obj .: "lamBody")
      <&> V.BAbs
    , V.GetField <$> obj .: "getFieldRec" <*> (obj .: "getFieldName" >>= decodeTagId)
      <&> V.BGetField
    , V.RecExtend
      <$> (obj .: "extendTag" >>= decodeTagId)
      <*> obj .: "extendVal" <*> obj .: "extendRest"
      <&> V.BRecExtend
    , V.Inject
      <$> (obj .: "injectTag" >>= decodeTagId)
      <*> obj .: "injectVal"
      <&> V.BInject
    , V.Case
      <$> (obj .: "caseTag" >>= decodeTagId)
      <*> obj .: "caseHandler"
      <*> obj .: "caseRest"
      <&> V.BCase
    , V.Nom
      <$> (obj .: "toNomId" >>= decodeIdent <&> T.NominalId)
      <*> obj .: "toNomVal"
      <&> V.BToNom
    , V.Nom
      <$> (obj .: "fromNomId" >>= decodeIdent <&> T.NominalId)
      <*> obj .: "fromNomVal"
      <&> V.BFromNom
    , decodeLeaf obj <&> V.BLeaf
    ] >>= traverse decodeVal

encodeExportedType :: Encoder Definition.ExportedType
encodeExportedType Definition.NoExportedType = Aeson.String "NoExportedType"
encodeExportedType (Definition.ExportedType scheme) = encodeScheme scheme

decodeExportedType :: Decoder Definition.ExportedType
decodeExportedType (Aeson.String "NoExportedType") = pure Definition.NoExportedType
decodeExportedType json = decodeScheme json <&> Definition.ExportedType

encodeDefBody :: Definition.Body (Val UUID) -> Aeson.Object
encodeDefBody (Definition.BodyBuiltin (Definition.Builtin name scheme)) =
    HashMap.fromList
    [ "builtin" .=
      Aeson.object
      [ "name" .= encodeFFIName name
      , "scheme" .= encodeScheme scheme
      ]
    ]
encodeDefBody (Definition.BodyExpr (Definition.Expr val typ frozenDefTypes)) =
    [ "val" .= encodeVal val
    , "typ" .= encodeExportedType typ
    ] ++
    encodeSquash Map.null "frozenDefTypes" (encodeIdentMap V.vvName encodeScheme)
    frozenDefTypes
    & HashMap.fromList

decodeDefBody :: Decoder (Definition.Body (Val UUID))
decodeDefBody =
    Aeson.withObject "DefBody" $ \obj ->
    jsum
    [ obj .: "builtin" >>= decodeBuiltin <&> Definition.BodyBuiltin
    , Definition.Expr
      <$> (obj .: "val" >>= decodeVal)
      <*> (obj .: "typ" >>= decodeExportedType)
      <*> decodeSquashed "frozenDefTypes" (decodeIdentMap V.Var decodeScheme) obj
      <&> Definition.BodyExpr
    ]
    where
        decodeBuiltin =
            Aeson.withObject "builtin" $
            \builtin ->
            Definition.Builtin
            <$> (builtin .: "name" >>= decodeFFIName)
            <*> (builtin .: "scheme" >>= decodeScheme)

encodeRepl :: Encoder (Val UUID)
encodeRepl val = Aeson.object [ "repl" .= encodeVal val ]

decodeRepl :: Decoder (Val UUID)
decodeRepl = Aeson.withObject "repl" $ \obj -> obj .: "repl" >>= decodeVal

insertField :: Aeson.ToJSON a => String -> a -> Aeson.Object -> Aeson.Object
insertField k v = HashMap.insert (fromString k) (Aeson.toJSON v)

encodeNominal :: Maybe Nominal -> Aeson.Object
encodeNominal Nothing = HashMap.empty -- opaque nominal
encodeNominal (Just (Nominal paramsMap scheme)) =
    ("scheme" .= encodeScheme scheme) :
    encodeSquash Map.null "typeParams"
    (encodeIdentMap T.typeParamId (encodeIdent . T.tvName)) paramsMap
    & HashMap.fromList

decodeNominal :: Decoder (Maybe Nominal)
decodeNominal json =
    ( Aeson.withObject "nominal" ?? json $ \obj ->
      Nominal
      <$> decodeSquashed "typeParams" (decodeIdentMap T.ParamId (fmap T.Var . decodeIdent)) obj
      <*> (obj .: "scheme" >>= decodeScheme)
    )
    -- TODO: maybe mark opaque nominals somehow? This
    -- will throw stuff under the rug:
    & optional

encodeNamed :: String -> (a -> Aeson.Object) -> ((Maybe String, Identifier), a) -> Aeson.Object
encodeNamed idAttrName encoder ((mName, ident), x) =
    encoder x
    & insertField idAttrName (encodeIdent ident)
    & maybe id (insertField "name") mName

decodeNamed :: String -> Decoder a -> Decoder ((Maybe String, Identifier), a)
decodeNamed idAttrName decoder =
    Aeson.withObject "named" $ \obj ->
    (,)
    <$> ( (,)
          <$> optional (obj .: "name")
          <*> (obj .: fromString idAttrName >>= decodeIdent)
        )
    <*> decoder (Aeson.Object obj)

encodeDef ::
    Encoder (Definition (Val UUID) (Anchors.PresentationMode, Maybe String, V.Var))
encodeDef (Definition body (presentationMode, mName, V.Var globalId)) =
    encodeNamed "def" encodeDefBody ((mName, globalId), body)
    & insertField "defPresentationMode" (encodePresentationMode presentationMode)
    & Aeson.Object

decodeDef ::
    Decoder
    (Definition (Val UUID)
     (Anchors.PresentationMode, Maybe String, V.Var))
decodeDef =
    Aeson.withObject "def" $ \obj ->
    do
        ((mName, globalId), body) <- decodeNamed "def" decodeDefBody (Aeson.Object obj)
        presentationMode <- obj .: "defPresentationMode" >>= decodePresentationMode
        Definition body (presentationMode, mName, V.Var globalId) & return

encodeTagOrder :: Int -> Aeson.Object
encodeTagOrder tagOrder = HashMap.fromList ["tagOrder" .= tagOrder]

decodeTagOrder :: Decoder Int
decodeTagOrder =
    Aeson.withObject "tagOrder" $ \obj ->
    obj .: "tagOrder"

encodeNamedTag :: Encoder (Int, Maybe String, T.Tag)
encodeNamedTag (tagOrder, mName, T.Tag ident) =
    encodeNamed "tag" encodeTagOrder ((mName, ident), tagOrder) & Aeson.Object

decodeNamedTag :: Decoder (Int, Maybe String, T.Tag)
decodeNamedTag json =
    do
        ((mName, tag), tagOrder) <- decodeNamed "tag" decodeTagOrder json
        return (tagOrder, mName, T.Tag tag)

encodeParamList :: Encoder Anchors.ParamList
encodeParamList = Aeson.toJSON . map encodeTagId

decodeParamList :: Decoder Anchors.ParamList
decodeParamList json = Aeson.parseJSON json >>= traverse decodeTagId

encodeMaybe :: Encoder a -> Encoder (Maybe a)
encodeMaybe encoder mVal = mVal <&> encoder & Aeson.toJSON

decodeMaybe :: Decoder a -> Decoder (Maybe a)
decodeMaybe decoder json = Aeson.parseJSON json >>= traverse decoder

encodeLam :: Aeson.ToJSON a => (a, Maybe Anchors.ParamList) -> Aeson.Object
encodeLam (lamI, mParamList) =
    HashMap.fromList
    [ "lamId" .= lamI
    , "lamFieldParams" .= encodeMaybe encodeParamList mParamList
    ]

decodeLam :: Aeson.FromJSON a => Decoder (a, Maybe Anchors.ParamList)
decodeLam =
    Aeson.withObject "lam" $ \obj ->
    do
        lamI <- obj .: "lamId"
        mParamList <- obj .: "lamFieldParams" >>= decodeMaybe decodeParamList
        return (lamI, mParamList)

encodeNamedLamVar ::
    Aeson.ToJSON a => Encoder (Maybe Anchors.ParamList, Maybe String, a, V.Var)
encodeNamedLamVar (mParamList, mName, lamI, V.Var ident) =
    encodeNamed "lamVar" encodeLam ((mName, ident), (lamI, mParamList)) & Aeson.Object

decodeNamedLamVar ::
    Aeson.FromJSON a => Decoder (Maybe Anchors.ParamList, Maybe String, a, V.Var)
decodeNamedLamVar json =
    do
        ((mName, ident), (lamI, mParamList)) <- decodeNamed "lamVar" decodeLam json
        return (mParamList, mName, lamI, V.Var ident)

encodeNamedNominal :: Encoder ((Maybe String, T.NominalId), Maybe Nominal)
encodeNamedNominal ((mName, T.NominalId nomId), nom) =
    encodeNamed "nom" encodeNominal ((mName, nomId), nom) & Aeson.Object

decodeNamedNominal :: Decoder ((Maybe String, T.NominalId), Maybe Nominal)
decodeNamedNominal json =
    decodeNamed "nom" decodeNominal json <&> _1 . _2 %~ T.NominalId
