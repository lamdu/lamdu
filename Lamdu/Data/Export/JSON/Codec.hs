-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE NoImplicitPrelude, LambdaCase, OverloadedStrings, PatternGuards, FlexibleContexts #-}
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
import           Control.Monad (unless)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer (WriterT(..))
import qualified Control.Monad.Trans.Writer as Writer
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.ByteString.Hex (showHexBytes, parseHexBytes)
import           Data.Either.Combinators (swapEither)
import qualified Data.HashMap.Strict as HashMap
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Data.UUID.Aeson ()
import           Data.UUID.Types (UUID)
import qualified Data.Vector as Vector
import           Lamdu.Calc.Identifier (Identifier, identHex, identFromHex)
import           Lamdu.Calc.Type (Type, Composite)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Constraints (Constraints(..), CompositeVarConstraints(..))
import           Lamdu.Calc.Type.FlatComposite (FlatComposite(..))
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Nominal (Nominal(..), NominalType(..))
import           Lamdu.Calc.Type.Scheme (Scheme(..))
import           Lamdu.Calc.Type.Vars (TypeVars(..))
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition

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

jsum :: [AesonTypes.Parser a] -> AesonTypes.Parser a
jsum parsers =
    parsers <&> toEither
    <&> swapEither & sequence <&> unlines & swapEither
    & fromEither

jsum' :: [Exhaustive a] -> Exhaustive a
jsum' = WriterT . jsum . map runWriterT

type Exhaustive a = WriterT [Text] AesonTypes.Parser a
type ExhaustiveDecoder a = Aeson.Object -> Exhaustive a

(.:) :: Aeson.FromJSON a => Aeson.Object -> String -> Exhaustive a
obj .: name =
    do
        Writer.tell [tName]
        lift (obj Aeson..: tName)
    where
        tName = fromString name

withObject :: String -> ExhaustiveDecoder a -> Decoder a
withObject msg act =
    Aeson.withObject msg $ \obj ->
    do
        (res, accessed) <- runWriterT (act obj)
        let unaccessed =
                Set.fromList (HashMap.keys obj) `Set.difference`
                Set.fromList accessed
                & Set.toList
        unless (null unaccessed) $
            fail $ "Object " ++ LBS8.unpack (AesonPretty.encodePretty obj) ++
            " has ignored fields: " ++ show unaccessed
        return res

decodeSquashed ::
    (Aeson.FromJSON j, Monoid a) =>
    String -> (j -> AesonTypes.Parser a) -> ExhaustiveDecoder a
decodeSquashed name decode o =
    jsum'
    [ o .: name >>= lift . decode
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
    withObject "Type" ?? json $ \o ->
    jsum'
    [ T.TFun
        <$> (o .: "funcParam" >>= lift . decodeType)
        <*> (o .: "funcResult" >>= lift . decodeType)
    , o .: "record" >>= lift . decodeComposite <&> T.TRecord
    , o .: "sum" >>= lift . decodeComposite <&> T.TSum
    , o .: "typeVar" >>= lift . decodeIdent <&> T.Var <&> T.TVar
    , do
          nomId <- o .: "nomId" >>= lift . decodeIdent <&> T.NominalId
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
    withObject "TypeVars" $ \obj ->
    do
        let getTVs name = decodeSquashed name decodeTVs obj
        tvs <-
            TypeVars
            <$> getTVs "typeVars"
            <*> getTVs "recordTypeVars"
            <*> getTVs "sumTypeVars"
        decodedConstraints <-
            decodeSquashed "constraints"
            ( withObject "constraints" $ \constraints ->
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
    withObject "scheme" $ \obj ->
    do
        (tvs, constraints) <- decodeSquashed "schemeBinders" decodeTypeVars obj
        typ <- obj .: "schemeType" >>= lift . decodeType
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

decodeLeaf :: ExhaustiveDecoder V.Leaf
decodeLeaf obj =
    jsum'
    [ leaf "hole" V.LHole
    , leaf "recEmpty" V.LRecEmpty
    , leaf "absurd" V.LAbsurd
    , obj .: "var" >>= lift . decodeIdent <&> V.Var <&> V.LVar
    , do
          primId <- obj .: "primId" >>= lift . decodeIdent <&> T.NominalId
          primBytes <- obj .: "primBytes" >>= lift . fromEither . parseHexBytes
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
    withObject "val" $ \obj ->
    Val
    <$> (obj .: "id")
    <*> decodeValBody obj

encodeValBody :: V.Body (Val UUID) -> AesonTypes.Object
encodeValBody body =
    case body <&> encodeVal of
    V.BApp (V.Apply func arg) ->
        HashMap.fromList ["applyFunc" .= func, "applyArg" .= arg]
    V.BLam (V.Lam (V.Var varId) res) ->
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

decodeValBody :: ExhaustiveDecoder (V.Body (Val UUID))
decodeValBody obj =
    jsum'
    [ V.Apply <$> obj .: "applyFunc" <*> obj .: "applyArg" <&> V.BApp
    , V.Lam <$> (obj .: "lamVar" >>= lift . decodeIdent <&> V.Var) <*> (obj .: "lamBody")
      <&> V.BLam
    , V.GetField <$> obj .: "getFieldRec" <*> (obj .: "getFieldName" >>= lift . decodeTagId)
      <&> V.BGetField
    , V.RecExtend
      <$> (obj .: "extendTag" >>= lift . decodeTagId)
      <*> obj .: "extendVal" <*> obj .: "extendRest"
      <&> V.BRecExtend
    , V.Inject
      <$> (obj .: "injectTag" >>= lift . decodeTagId)
      <*> obj .: "injectVal"
      <&> V.BInject
    , V.Case
      <$> (obj .: "caseTag" >>= lift . decodeTagId)
      <*> obj .: "caseHandler"
      <*> obj .: "caseRest"
      <&> V.BCase
    , V.Nom
      <$> (obj .: "toNomId" >>= lift . decodeIdent <&> T.NominalId)
      <*> obj .: "toNomVal"
      <&> V.BToNom
    , V.Nom
      <$> (obj .: "fromNomId" >>= lift . decodeIdent <&> T.NominalId)
      <*> obj .: "fromNomVal"
      <&> V.BFromNom
    , decodeLeaf obj <&> V.BLeaf
    ] >>= traverse (lift . decodeVal)

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

decodeDefBody :: ExhaustiveDecoder (Definition.Body (Val UUID))
decodeDefBody obj =
    jsum'
    [ obj .: "builtin" >>= lift . decodeBuiltin <&> Definition.BodyBuiltin
    , Definition.Expr
      <$> (obj .: "val" >>= lift . decodeVal)
      <*> (obj .: "typ" >>= lift . decodeExportedType)
      <*> decodeSquashed "frozenDefTypes" (decodeIdentMap V.Var decodeScheme) obj
      <&> Definition.BodyExpr
    ]
    where
        decodeBuiltin =
            withObject "builtin" $
            \builtin ->
            Definition.Builtin
            <$> (builtin .: "name" >>= lift . decodeFFIName)
            <*> (builtin .: "scheme" >>= lift . decodeScheme)

encodeRepl :: Encoder (Val UUID)
encodeRepl val = Aeson.object [ "repl" .= encodeVal val ]

decodeRepl :: Decoder (Val UUID)
decodeRepl = withObject "repl" $ \obj -> obj .: "repl" >>= lift . decodeVal

insertField :: Aeson.ToJSON a => String -> a -> Aeson.Object -> Aeson.Object
insertField k v = HashMap.insert (fromString k) (Aeson.toJSON v)

encodeNominalType :: Encoder NominalType
encodeNominalType (NominalType scheme) = encodeScheme scheme
encodeNominalType OpaqueNominal = "OpaqueNominal"

decodeNominalType :: Decoder NominalType
decodeNominalType (Aeson.String "OpaqueNominal") = pure OpaqueNominal
decodeNominalType json = decodeScheme json <&> NominalType

encodeNominal :: Nominal -> Aeson.Object
encodeNominal (Nominal paramsMap nominalType) =
    "nomType" .= encodeNominalType nominalType :
    encodeSquash Map.null "typeParams"
    (encodeIdentMap T.typeParamId (encodeIdent . T.tvName)) paramsMap
    & HashMap.fromList

decodeNominal :: ExhaustiveDecoder Nominal
decodeNominal obj =
    Nominal
    <$> decodeSquashed "typeParams" (decodeIdentMap T.ParamId (fmap T.Var . decodeIdent)) obj
    <*> (obj .: "nomType" >>= lift . decodeNominalType)

encodeNamed :: String -> (a -> Aeson.Object) -> ((Maybe String, Identifier), a) -> Aeson.Object
encodeNamed idAttrName encoder ((mName, ident), x) =
    encoder x
    & insertField idAttrName (encodeIdent ident)
    & maybe id (insertField "name") mName

decodeNamed :: String -> ExhaustiveDecoder a -> ExhaustiveDecoder ((Maybe String, Identifier), a)
decodeNamed idAttrName decoder obj =
    (,)
    <$> ( (,)
          <$> optional (obj .: "name")
          <*> (obj .: fromString idAttrName >>= lift . decodeIdent)
        )
    <*> decoder obj

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
    withObject "def" $ \obj ->
    do
        ((mName, globalId), body) <- decodeNamed "def" decodeDefBody obj
        presentationMode <- obj .: "defPresentationMode" >>= lift . decodePresentationMode
        Definition body (presentationMode, mName, V.Var globalId) & return

encodeTagOrder :: Int -> Aeson.Object
encodeTagOrder tagOrder = HashMap.fromList ["tagOrder" .= tagOrder]

decodeTagOrder :: ExhaustiveDecoder Int
decodeTagOrder obj = obj .: "tagOrder"

encodeNamedTag :: Encoder (Int, Maybe String, T.Tag)
encodeNamedTag (tagOrder, mName, T.Tag ident) =
    encodeNamed "tag" encodeTagOrder ((mName, ident), tagOrder) & Aeson.Object

decodeNamedTag :: Decoder (Int, Maybe String, T.Tag)
decodeNamedTag json =
    do
        ((mName, tag), tagOrder) <-
            withObject "tag" ?? json $ decodeNamed "tag" decodeTagOrder
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

decodeLam :: Aeson.FromJSON a => ExhaustiveDecoder (a, Maybe Anchors.ParamList)
decodeLam obj =
    do
        lamI <- obj .: "lamId"
        mParamList <- obj .: "lamFieldParams" >>= lift . decodeMaybe decodeParamList
        return (lamI, mParamList)

encodeNamedLamVar ::
    Aeson.ToJSON a => Encoder (Maybe Anchors.ParamList, Maybe String, a, V.Var)
encodeNamedLamVar (mParamList, mName, lamI, V.Var ident) =
    encodeNamed "lamVar" encodeLam ((mName, ident), (lamI, mParamList)) & Aeson.Object

decodeNamedLamVar ::
    Aeson.FromJSON a => Decoder (Maybe Anchors.ParamList, Maybe String, a, V.Var)
decodeNamedLamVar json =
    do
        ((mName, ident), (lamI, mParamList)) <-
            withObject "lam" ?? json $ decodeNamed "lamVar" decodeLam
        return (mParamList, mName, lamI, V.Var ident)

encodeNamedNominal :: Encoder ((Maybe String, T.NominalId), Nominal)
encodeNamedNominal ((mName, T.NominalId nomId), nom) =
    encodeNamed "nom" encodeNominal ((mName, nomId), nom) & Aeson.Object

decodeNamedNominal :: Decoder ((Maybe String, T.NominalId), Nominal)
decodeNamedNominal json =
    withObject "nom" (decodeNamed "nom" decodeNominal) json
    <&> _1 . _2 %~ T.NominalId
