-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE NoImplicitPrelude, LambdaCase, OverloadedStrings #-}
module Lamdu.Data.Export.JSON.Codec
    ( Encoder, encodeNamedTag, encodeRepl, encodeDef, encodeNamedNominal, encodeArray
    , Decoder, decodeNamedTag, decodeRepl, decodeDef, decodeNamedNominal, decodeArray
    , Encoded, runDecoder
    ) where

import           Control.Applicative (Alternative(..), optional)
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
    [ Aeson.parseJSON json
      >>= \(encodedFields, encodedIdent) ->
      do
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
encodeType (T.TFun a b) = Aeson.object ["TFun" .= encodeArray (map encodeType [a, b])]
encodeType (T.TRecord composite) = Aeson.object ["record" .= encodeComposite composite]
encodeType (T.TSum composite) = Aeson.object ["sum" .= encodeComposite composite]
encodeType (T.TVar (T.Var name)) = Aeson.object ["TVar" .= encodeIdent name]
encodeType (T.TInst tId params) =
    Aeson.object
    [ "TId" .= encodeIdent (T.nomId tId)
    , "TParams" .= encodeIdentMap T.typeParamId encodeType params
    ]

decodeType :: Decoder Type
decodeType (Aeson.String "TODO:TPrim") = error "TODO:TPrim"
decodeType json =
    Aeson.withObject "Type" ?? json $ \o ->
    jsum
    [ o .: "TFun" >>= \(a, b) -> T.TFun <$> decodeType a <*> decodeType b
    , o .: "record" >>= decodeComposite <&> T.TRecord
    , o .: "sum" >>= decodeComposite <&> T.TSum
    , o .: "TVar" >>= decodeIdent <&> T.Var <&> T.TVar
    , do
          nomId <- o .: "TId" >>= decodeIdent <&> T.NominalId
          params <- o .: "TParams" >>= decodeIdentMap T.ParamId decodeType
          T.TInst nomId params & pure
    ]

encodeTypeVars :: Encoder (TypeVars, Constraints)
encodeTypeVars (TypeVars tvs rtvs stvs, Constraints productConstraints sumConstraints) =
    Aeson.object
    [ "tvs" .= encodeTVs tvs
    , "rtvs" .= encodeTVs rtvs
    , "stvs" .= encodeTVs stvs
    , "constraints" .= Aeson.object
      [ "rtvs" .= encodeConstraints productConstraints
      , "stvs" .= encodeConstraints sumConstraints
      ]
    ]
    where
        encodeConstraints (CompositeVarConstraints constraints) =
            encodeIdentMap T.tvName (map (encodeIdent . T.tagName) . Set.toList)
            constraints
        encodeTVs = Aeson.toJSON . map (encodeIdent . T.tvName) . Set.toList

decodeTypeVars :: Decoder (TypeVars, Constraints)
decodeTypeVars =
    Aeson.withObject "TypeVars" $ \obj ->
    do
        let getTVs name = obj .: name >>= decodeTVs
        tvs <- TypeVars <$> getTVs "tvs" <*> getTVs "rtvs" <*> getTVs "stvs"
        constraints <-
            obj .: "constraints" >>=
            ( Aeson.withObject "constraints" $ \constraints ->
              let  getCs name = constraints .: name >>= decodeConstraints <&> CompositeVarConstraints
              in   Constraints <$> getCs "rtvs" <*> getCs "stvs"
            )
        return (tvs, constraints)
    where
        decodeForbiddenFields json =
            traverse decodeIdent json <&> map T.Tag <&> Set.fromList
        decodeConstraints = decodeIdentMap T.Var decodeForbiddenFields
        decodeTV = fmap T.Var . decodeIdent
        decodeTVs = fmap Set.fromList . traverse decodeTV

encodeScheme :: Encoder Scheme
encodeScheme (Scheme tvs constraints typ) =
    Aeson.object
    [ "schemeBinders" .= encodeTypeVars (tvs, constraints)
    , "schemeType" .= encodeType typ
    ]

decodeScheme :: Decoder Scheme
decodeScheme =
    Aeson.withObject "scheme" $ \obj ->
    do
        (tvs, constraints) <- obj .: "schemeBinders" >>= decodeTypeVars
        typ <- obj .: "schemeType" >>= decodeType
        Scheme tvs constraints typ & return

encodeLeaf :: Encoder V.Leaf
encodeLeaf V.LHole = Aeson.String "hole"
encodeLeaf V.LRecEmpty = Aeson.String "()"
encodeLeaf V.LAbsurd = Aeson.String "absurd"
encodeLeaf (V.LLiteral (V.PrimVal (T.NominalId primId) primBytes)) =
    Aeson.object
    [ "primId" .= encodeIdent primId
    , "primBytes" .= showHexBytes primBytes
    ]
encodeLeaf (V.LVar (V.Var var)) = encodeIdent var

decodeLeaf :: Decoder V.Leaf
decodeLeaf (Aeson.String "hole") = pure V.LHole
decodeLeaf (Aeson.String "()") = pure V.LRecEmpty
decodeLeaf (Aeson.String "absurd") = pure V.LAbsurd
decodeLeaf json =
    jsum
    [ Aeson.withObject "literal" ?? json $ \obj ->
      do
          primId <- obj .: "primId" >>= decodeIdent <&> T.NominalId
          primBytes <- obj .: "primBytes" >>= fromEither . parseHexBytes
          V.PrimVal primId primBytes & pure
      <&> V.LLiteral
    , decodeIdent json <&> V.Var <&> V.LVar
    ]

-- TODO: Should we export the UUIDs of subexprs?
encodeVal :: Aeson.ToJSON a => Encoder (Val a)
encodeVal (Val pl body) = Aeson.toJSON (pl, encodeValBody body)

decodeVal :: Aeson.FromJSON a => Decoder (Val a)
decodeVal json =
    do
        (pl, body) <- Aeson.parseJSON json
        Val pl <$> decodeValBody body

encodeValBody :: Aeson.ToJSON a => V.Body (Val a) -> AesonTypes.Value
encodeValBody body =
    case body <&> encodeVal of
    V.BApp (V.Apply func arg) ->
        Aeson.object ["applyFunc" .= func, "applyArg" .= arg]
    V.BAbs (V.Lam (V.Var varId) res) ->
        Aeson.object ["lamVar" .= encodeIdent varId, "lamBody" .= res]
    V.BGetField (V.GetField reco tag) ->
        Aeson.object ["getFieldRec" .= reco, "getFieldName" .= encodeTagId tag]
    V.BRecExtend (V.RecExtend tag val rest) ->
        Aeson.object
        ["extendTag" .= encodeTagId tag, "extendVal" .= val, "extendRest" .= rest]
    V.BInject (V.Inject tag val) ->
        Aeson.object ["injectTag" .= encodeTagId tag, "injectVal" .= val]
    V.BCase (V.Case tag handler restHandler) ->
        Aeson.object ["caseTag" .= encodeTagId tag, "caseHandler" .= handler, "caseRest" .= restHandler]
    V.BToNom (V.Nom (T.NominalId nomId) val) ->
        Aeson.object ["toNomId" .= encodeIdent nomId, "toNomVal" .= val]
    V.BFromNom (V.Nom (T.NominalId nomId) val) ->
        Aeson.object ["fromNomId" .= encodeIdent nomId, "fromNomVal" .= val]
    V.BLeaf leaf -> encodeLeaf leaf

decodeValBody :: AesonTypes.FromJSON a => Encoded -> AesonTypes.Parser (V.Body (Val a))
decodeValBody json =
    (decodeLeaf json <&> V.BLeaf)
    <|>
    ( Aeson.withObject "Val" ?? json $ \obj ->
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
      ] >>= traverse decodeVal
    )

encodeExportedType :: Encoder Definition.ExportedType
encodeExportedType Definition.NoExportedType = Aeson.String "NoExportedType"
encodeExportedType (Definition.ExportedType scheme) = encodeScheme scheme

decodeExportedType :: Decoder Definition.ExportedType
decodeExportedType (Aeson.String "NoExportedType") = pure Definition.NoExportedType
decodeExportedType json = decodeScheme json <&> Definition.ExportedType

encodeDefBody :: Encoder (Definition.Body (Val UUID))
encodeDefBody (Definition.BodyBuiltin (Definition.Builtin name scheme)) =
    Aeson.object
    [ "builtin" .=
      Aeson.object
      [ "name" .= encodeFFIName name
      , "scheme" .= encodeScheme scheme
      ]
    ]
encodeDefBody (Definition.BodyExpr (Definition.Expr val typ frozenDefTypes)) =
    Aeson.object
    [ "val" .= encodeVal val
    , "typ" .= encodeExportedType typ
    , "frozenDefTypes" .= encodedFrozen
    ]
    where
        encodedFrozen = encodeIdentMap V.vvName encodeScheme frozenDefTypes

decodeDefBody :: Decoder (Definition.Body (Val UUID))
decodeDefBody =
    Aeson.withObject "DefBody" $ \obj ->
    jsum
    [ obj .: "builtin" >>= decodeBuiltin <&> Definition.BodyBuiltin
    , Definition.Expr
      <$> (obj .: "val" >>= decodeVal)
      <*> (obj .: "typ" >>= decodeExportedType)
      <*> (obj .: "frozenDefTypes" >>= decodeIdentMap V.Var decodeScheme)
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

insertField :: Aeson.ToJSON a => String -> a -> Aeson.Value -> Aeson.Value
insertField k v (Aeson.Object obj) =
    Aeson.Object (HashMap.insert (fromString k) (Aeson.toJSON v) obj)
insertField _ _ _ = error "insertField: Expecting object"

encodeUnit :: Encoder ()
encodeUnit () = Aeson.object []

decodeUnit :: Decoder ()
decodeUnit _ = pure ()

encodeNominal :: Encoder Nominal
encodeNominal (Nominal paramsMap scheme) =
    Aeson.object
    [ "typeParams" .= encodeIdentMap T.typeParamId (encodeIdent . T.tvName) paramsMap
    , "scheme" .= encodeScheme scheme
    ]

decodeNominal :: Decoder Nominal
decodeNominal =
    Aeson.withObject "nominal" $ \obj ->
    Nominal
    <$> (obj .: "typeParams" >>= decodeIdentMap T.ParamId (fmap T.Var . decodeIdent))
    <*> (obj .: "scheme" >>= decodeScheme)

encodeNamed :: String -> Encoder a -> Encoder ((Maybe String, Identifier), a)
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

encodeDef :: Encoder (Definition (Val UUID) (Maybe String, V.Var))
encodeDef (Definition body (mName, V.Var globalId)) =
    encodeNamed "def" encodeDefBody ((mName, globalId), body)

decodeDef :: Decoder (Definition (Val UUID) (Maybe String, V.Var))
decodeDef json =
    decodeNamed "def" decodeDefBody json <&>
    \((mName, globalId), body) ->
    Definition body (mName, V.Var globalId)

encodeNamedTag :: Encoder (Maybe String, T.Tag)
encodeNamedTag (mName, T.Tag ident) =
    encodeNamed "tag" encodeUnit ((mName, ident), ())

decodeNamedTag :: Decoder (Maybe String, T.Tag)
decodeNamedTag json =
    decodeNamed "tag" decodeUnit json <&> fst <&> _2 %~ T.Tag

encodeNamedNominal :: Encoder ((Maybe String, T.NominalId), Nominal)
encodeNamedNominal ((mName, T.NominalId nomId), nom) =
    encodeNamed "nom" encodeNominal ((mName, nomId), nom)

decodeNamedNominal :: Decoder ((Maybe String, T.NominalId), Nominal)
decodeNamedNominal json =
    decodeNamed "nom" decodeNominal json <&> _1 . _2 %~ T.NominalId
