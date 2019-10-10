-- | JSON encoder/decoder for Lamdu types
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Lamdu.Data.Export.JSON.Codec
    ( TagOrder
    , Entity(..), _EntitySchemaVersion, _EntityRepl, _EntityDef, _EntityTag, _EntityNominal, _EntityLamVar
    ) where

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
import           Hyper (Ann(..), htraverse1, Tree, Pure(..), _Pure)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (ToNom(..), NominalDecl(..), NominalInst(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import qualified Hyper.Type.AST.Row as Row
import           Hyper.Type.AST.Scheme (Scheme(..), QVars(..), QVarInstances(..), _QVarInstances)
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

type Encoded = Aeson.Value

type Encoder a = a -> Encoded
type Decoder a = Encoded -> AesonTypes.Parser a

type TagOrder = Int

data Entity
    = EntitySchemaVersion Int
    | EntityRepl (Definition.Expr (Val UUID))
    | EntityDef (Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var))
    | EntityTag T.Tag Tag
    | EntityNominal T.Tag T.NominalId (Maybe (Tree Pure (NominalDecl T.Type)))
    | EntityLamVar (Maybe Meta.ParamList) T.Tag UUID V.Var
Lens.makePrisms ''Entity

instance AesonTypes.ToJSON Entity where
    toJSON (EntitySchemaVersion ver) = encodeSchemaVersion ver
    toJSON (EntityRepl x) = encodeRepl x
    toJSON (EntityDef def) = encodeDef def
    toJSON (EntityTag tid tdata) = encodeNamedTag (tid, tdata)
    toJSON (EntityNominal tag nomId nom) = encodeTaggedNominal ((tag, nomId), nom)
    toJSON (EntityLamVar mParamList tag lamI var) = encodeTaggedLamVar (mParamList, tag, lamI, var)

instance AesonTypes.FromJSON Entity where
    parseJSON =
        decodeVariant "entity"
        [ ("repl" , fmap EntityRepl . decodeRepl)
        , ("def"  , fmap EntityDef  . decodeDef)
        , ("tagOrder", fmap (uncurry EntityTag) . decodeNamedTag)
        , ("nom"  , fmap (\((tag, nomId), nom) -> EntityNominal tag nomId nom) . decodeTaggedNominal)
        , ("lamId", fmap (uncurry4 EntityLamVar) . decodeTaggedLamVar)
        , ("schemaVersion", fmap EntitySchemaVersion . decodeSchemaVersion)
        ]

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

encodeFlatComposite :: Encoder (Tree (Row.FlatRowExtends T.Tag T.Type T.Row) Pure)
encodeFlatComposite (Row.FlatRowExtends fields rest) =
    case rest ^. _Pure of
    T.REmpty -> encodedFields fields
    T.RVar (T.Var name) -> AesonTypes.toJSON [encodedFields fields, encodeIdent name]
    T.RExtend{} -> error "RExtend in tail of flat row"
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

decodeFlatComposite :: Decoder (Tree (Row.FlatRowExtends T.Tag T.Type T.Row) Pure)
decodeFlatComposite json =
    jsum
    [ do
          (encodedFields, encodedIdent) <- Aeson.parseJSON json
          fields <- decodeFields encodedFields
          tv <- decodeIdent encodedIdent <&> T.Var
          Row.FlatRowExtends fields (_Pure . T._RVar # tv) & pure

    , Aeson.parseJSON json
      >>= decodeFields
      <&> (`Row.FlatRowExtends` (_Pure # T.REmpty))
    ]
    where
        decodeFields = decodeIdentMap T.Tag decodeType

encodeComposite :: Encoder (Tree Pure T.Row)
encodeComposite x = encodeFlatComposite (x ^. T.flatRow)

decodeComposite :: Decoder (Tree Pure T.Row)
decodeComposite x =
    decodeFlatComposite x <&> unflatten
    where
        unflatten (Row.FlatRowExtends extends rest) =
            -- It appears that we've accidentally relies on sorted order of fields.
            -- Using a foldl here will cause typing "1+2" appear as "2+1"!!
            foldr f rest (Map.toList extends)
            where
                f (key, val) acc = _Pure . T._RExtend # RowExtend key val acc

encodeType :: Encoder (Tree Pure T.Type)
encodeType t =
    case t ^. _Pure of
    T.TFun (FuncType a b) -> ["funcParam" .= encodeType a, "funcResult" .= encodeType b]
    T.TRecord composite   -> ["record" .= encodeComposite composite]
    T.TVariant composite  -> ["variant" .= encodeComposite composite]
    T.TVar (T.Var name)   -> ["typeVar" .= encodeIdent name]
    T.TInst (NominalInst tId params) ->
        ("nomId" .= encodeIdent (T.nomId tId)) :
        encodeSquash "nomTypeArgs" (encodeIdentMap T.tvName encodeType) (params ^. T.tType . _QVarInstances) ++
        encodeSquash "nomRowArgs" (encodeIdentMap T.tvName encodeComposite) (params ^. T.tRow . _QVarInstances)
    & Aeson.object

decodeType :: Decoder (Tree Pure T.Type)
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

encodeCompositeVarConstraints :: T.RConstraints -> [Encoded]
encodeCompositeVarConstraints (T.RowConstraints forbidden scopeLevel)
    | scopeLevel == mempty =
        Set.toList forbidden
        <&> T.tagName
        <&> encodeIdent
    | otherwise =
        -- We only encode top-level types, no skolem escape considerations...
        error "encodeCompositeVarConstraints does not support inner-scoped types"

decodeCompositeConstraints ::
    [Encoded] -> AesonTypes.Parser T.RConstraints
decodeCompositeConstraints json =
    traverse decodeIdent json <&> map T.Tag <&> Set.fromList
    <&> (`T.RowConstraints` mempty)

encodeTypeVars :: Tree T.Types QVars -> [AesonTypes.Pair]
encodeTypeVars (T.Types (QVars tvs) (QVars rvs)) =
    encodeSquash "typeVars"
    (Aeson.toJSON . map (encodeIdent . T.tvName) . Set.toList)
    (Map.keysSet tvs)
    ++
    encodeSquash "rowVars"
    (encodeIdentMap T.tvName encodeCompositeVarConstraints)
    rvs

decodeTypeVars :: Aeson.Object -> AesonTypes.Parser (Tree T.Types QVars)
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

encodeScheme :: Encoder (Tree Pure T.Scheme)
encodeScheme (Pure (Scheme tvs typ)) =
    ("schemeType" .= encodeType typ) :
    encodeTypeVars tvs
    & Aeson.object

decodeScheme :: Decoder (Tree Pure T.Scheme)
decodeScheme =
    Aeson.withObject "scheme" $ \obj ->
    do
        tvs <- decodeTypeVars obj
        typ <- obj .: "schemeType" >>= decodeType
        _Pure # Scheme tvs typ & pure

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

encodeValBody :: Tree V.Term (Ann (Const UUID)) -> AesonTypes.Object
encodeValBody body =
    case body & htraverse1 %~ Lens.Const . encodeVal of
    V.BApp (V.App func arg) ->
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

decodeValBody :: AesonTypes.Object -> AesonTypes.Parser (Tree V.Term (Ann (Const UUID)))
decodeValBody obj =
    jsum
    [ V.App
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
    ] >>= htraverse1 (decodeVal . (^. Lens._Wrapped . Lens._Wrapped))
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
                (frozenDeps ^. depsGlobalTypes) ++
            encodeSquash "nominals"
                (encodeIdentMap T.nomId encodeNominal)
                (frozenDeps ^. depsNominals)

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
encodeRepl defExpr = Aeson.object [ "repl" .= encodeDefExpr defExpr ]

decodeRepl :: Aeson.Object -> AesonTypes.Parser (Definition.Expr (Val UUID))
decodeRepl obj =
    obj .: "repl" >>= Aeson.withObject "defExpr" decodeDefExpr

insertField :: Aeson.ToJSON a => Text -> a -> Aeson.Object -> Aeson.Object
insertField k v = HashMap.insert k (Aeson.toJSON v)

encodeNominal :: Tree Pure (NominalDecl T.Type) -> Aeson.Object
encodeNominal (Pure (NominalDecl params nominalType)) =
    ("nomType" .= encodeScheme (_Pure # nominalType))
    : encodeTypeVars params
    & HashMap.fromList

decodeNominal :: Aeson.Object -> AesonTypes.Parser (Tree Pure (NominalDecl T.Type))
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
encodeTagOrder tagOrder = HashMap.fromList ["tagOrder" .= tagOrder]

encodeOp :: Tag.OpName -> [AesonTypes.Pair]
encodeOp Tag.NotAnOp = []
encodeOp (Tag.OpUni x) = [("op", Aeson.String x)]
encodeOp (Tag.OpDir (Tag.DirOp l r)) =
    [("op", Aeson.Array (Vector.fromList [Aeson.String l, Aeson.String r]))]

encodeNamedTag :: Encoder (T.Tag, Tag)
encodeNamedTag (T.Tag ident, Tag order op names) =
    (encodeTagOrder order
    & insertField "tag" (encodeIdent ident))
    <> HashMap.fromList (encodeSquash "names" id names <> encodeOp op)
    & Aeson.Object

decodeOpName :: Maybe Aeson.Value -> AesonTypes.Parser Tag.OpName
decodeOpName Nothing = pure Tag.NotAnOp
decodeOpName (Just (Aeson.String x)) = Tag.OpUni x & pure
decodeOpName (Just (Aeson.Array x)) =
    case x ^.. traverse of
    [Aeson.String l, Aeson.String r] ->
        Tag.DirOp l r & Tag.OpDir & pure
    _ -> fail ("unexpected op names:" <> show x)
decodeOpName x = fail ("unexpected op name: " <> show x)

decodeNamedTag :: Aeson.Object -> AesonTypes.Parser (T.Tag, Tag)
decodeNamedTag obj =
    (,)
    <$> (obj .: "tag" >>= decodeIdent <&> T.Tag)
    <*>
    ( Tag
        <$> (obj .: "tagOrder")
        <*> decodeOpName (obj ^. Lens.at "op")
        <*> decodeSquashed "names" Aeson.parseJSON obj
    )

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

encodeTaggedNominal :: Encoder ((T.Tag, T.NominalId), Maybe (Tree Pure (NominalDecl T.Type)))
encodeTaggedNominal ((tag, T.NominalId nomId), mNom) =
    foldMap encodeNominal mNom
    & Lens.at "nom" ?~ encodeIdent nomId
    & Lens.at "tag" ?~ encodeTagId tag
    & Aeson.Object

decodeTaggedNominal :: Aeson.Object -> AesonTypes.Parser ((T.Tag, T.NominalId), Maybe (Tree Pure (NominalDecl T.Type)))
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
