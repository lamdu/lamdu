-- | Import/Export JSON support
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase, TemplateHaskell #-}
module Lamdu.Data.Export.JSON
    ( export
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators hiding ((.=))
import           Control.Lens.Tuple
import           Control.Monad (unless)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Writer (WriterT(..))
import qualified Control.Monad.Trans.Writer as Writer
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.ByteString.Hex (showHexBytes)
import           Data.Foldable (traverse_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Data.Vector as Vector
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Constraints (Constraints(..), CompositeVarConstraints(..))
import           Lamdu.Expr.FlatComposite (FlatComposite(..))
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Identifier (Identifier, identHex)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.Scheme (Scheme(..))
import           Lamdu.Expr.Type (Type, Composite)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.TypeVars (TypeVars(..))
import           Lamdu.Expr.UniqueId (ToGuid)
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

type T = Transaction

data Visited = Visited
    { _visitedDefs :: Set V.Var
    , _visitedTags :: Set T.Tag
    }
Lens.makeLenses ''Visited

type M = WriterT [Aeson.Value] (StateT Visited (T ViewM))

array :: [Aeson.Value] -> Aeson.Value
array = Aeson.Array . Vector.fromList

encodeFFIName :: Definition.FFIName -> Aeson.Value
encodeFFIName (Definition.FFIName modulePath name) = modulePath ++ [name] & Aeson.toJSON

encodeIdent :: Identifier -> Aeson.Value
encodeIdent = Aeson.toJSON . identHex

encodeIdentMap :: Aeson.ToJSON b => (k -> Identifier) -> (a -> b) -> Map k a -> Aeson.Value
encodeIdentMap getIdent encode =
    Aeson.toJSON . Map.mapKeys (identHex . getIdent) . Map.map encode

encodeTag :: T.Tag -> Aeson.Value
encodeTag = Aeson.toJSON . identHex . T.tagName

encodeFlatComposite :: FlatComposite p -> Aeson.Value
encodeFlatComposite = \case
    FlatComposite fields Nothing -> encodedFields fields
    FlatComposite fields (Just (T.Var name)) ->
        array [encodedFields fields, encodeIdent name]
    where
        encodedFields = encodeIdentMap T.tagName encodeType

encodeComposite :: Composite p -> Aeson.Value
encodeComposite = encodeFlatComposite . FlatComposite.fromComposite

encodeType :: Type -> Aeson.Value
encodeType (T.TPrim _) = Aeson.String "TODO:TPrim"
encodeType (T.TFun a b) = Aeson.object ["TFun" .= array (map encodeType [a, b])]
encodeType (T.TRecord composite) = Aeson.object ["record" .= encodeComposite composite]
encodeType (T.TSum composite) = Aeson.object ["sum" .= encodeComposite composite]
encodeType (T.TVar (T.Var name)) = Aeson.object ["TVar" .= encodeIdent name]
encodeType (T.TInst tId params) =
    Aeson.object
    [ "TId" .= encodeIdent (T.nomId tId)
    , "Params" .= encodeIdentMap T.typeParamId encodeType params
    ]

encodeTypeVars :: TypeVars -> Constraints -> Aeson.Value
encodeTypeVars (TypeVars tvs rtvs stvs) (Constraints productConstraints sumConstraints) =
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

encodeScheme :: Scheme -> Aeson.Value
encodeScheme (Scheme tvs constraints typ) =
    Aeson.object
    [ "schemeBinders" .= encodeTypeVars tvs constraints
    , "schemeType" .= encodeType typ
    ]

encodeLeaf :: V.Leaf -> Aeson.Value
encodeLeaf V.LHole = Aeson.String "hole"
encodeLeaf V.LRecEmpty = Aeson.object []
encodeLeaf V.LAbsurd = Aeson.String "absurd"
encodeLeaf (V.LVar (V.Var var)) = encodeIdent var
encodeLeaf (V.LLiteral (V.PrimVal (T.PrimId primId) primBytes)) =
    Aeson.object
    [ "primId" .= identHex primId
    , "primBytes" .= showHexBytes primBytes
    ]

-- TODO: Should we export the Guids of subexprs?
encodeVal :: Val (ValI m) -> Aeson.Value
encodeVal (Val _ body) =
    case body <&> encodeVal of
    V.BLeaf leaf -> encodeLeaf leaf
    V.BApp (V.Apply func arg) ->
        Aeson.object ["applyFunc" .= func, "applyArg" .= arg]
    V.BAbs (V.Lam (V.Var varId) res) ->
        Aeson.object ["lamVar" .= identHex varId, "lamBody" .= res]
    V.BGetField (V.GetField reco tag) ->
        Aeson.object ["getFieldRec" .= reco, "getFieldName" .= encodeTag tag]
    V.BRecExtend (V.RecExtend tag val rest) ->
        Aeson.object
        ["extendTag" .= encodeTag tag, "extendVal" .= val, "extendRest" .= rest]
    V.BInject (V.Inject tag val) ->
        Aeson.object ["injectTag" .= encodeTag tag, "injectVal" .= val]
    V.BCase (V.Case tag handler restHandler) ->
        Aeson.object ["caseTag" .= encodeTag tag, "caseHandler" .= handler, "caseRest" .= restHandler]
    V.BToNom (V.Nom (T.NominalId nomId) val) ->
        Aeson.object ["toNomId" .= encodeIdent nomId, "toNomVal" .= val]
    V.BFromNom (V.Nom (T.NominalId nomId) val) ->
        Aeson.object ["fromNomId" .= encodeIdent nomId, "fromNomVal" .= val]

encodeExportedType :: Definition.ExportedType -> Aeson.Value
encodeExportedType Definition.NoExportedType = Aeson.String "NoExportedType"
encodeExportedType (Definition.ExportedType scheme) = encodeScheme scheme

encodeDefBody :: Definition.Body (Val (ValI m)) -> Aeson.Value
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

run :: M a -> T ViewM (a, Aeson.Value)
run act =
    act
    & runWriterT
    <&> _2 %~ array
    & (`State.evalStateT` Visited mempty mempty)

trans :: T ViewM a -> M a
trans = lift . lift

withVisited :: Lens.Contains b => Lens.ALens' Visited b -> Lens.Index b -> M () -> M ()
withVisited l x act =
    do
        alreadyVisited <- Lens.use (Lens.cloneLens l . Lens.contains x)
        unless alreadyVisited $
            do
                Lens.assign (Lens.cloneLens l . Lens.contains x) True
                act

replIRef :: IRef ViewM (ValI ViewM)
replIRef = DbLayout.repl DbLayout.codeIRefs

readAssocName :: ToGuid a => a -> M (Maybe String)
readAssocName x =
    do
        name <- Anchors.assocNameRef x & Transaction.getP & trans
        return $
            if null name
            then Nothing
            else Just name

tell :: Aeson.Value -> M ()
tell = Writer.tell . (: [])

exportTag :: T.Tag -> M ()
exportTag tag =
    do
        mName <- readAssocName tag
        tell $ Aeson.object $ concat
            [ [ "tag" .= encodeTag tag ]
            , [ "name" .= name | Just name <- [mName] ]
            ]
    & withVisited visitedTags tag

recurse :: Val a -> M ()
recurse val =
    do
        -- TODO: Add a recursion on all ExprLens.subExprs -- that have
        -- a Lam inside them, on the "Var" to export all the var names
        -- too?  OR: alternatively, remove the "name" crap and just
        -- add the set of all Guids we ever saw to the WriterT, and
        -- export all associated names/data of all!
        val ^.. ExprLens.valGlobals mempty & traverse_ exportDef
        val ^.. ExprLens.valTags & traverse_ exportTag

exportDef :: V.Var -> M ()
exportDef globalId =
    do
        mName <- readAssocName globalId
        def <- ExprIRef.defI globalId & Load.loadDef & trans
        let encodedBody =
                def ^. Definition.defBody
                <&> Lens.mapped %~ Property.value
                & encodeDefBody

        traverse_ recurse (def ^. Definition.defBody)

        tell $ Aeson.object $ concat
            [ [ "def" .= identHex (V.vvName globalId) ]
            , [ "name" .= name | Just name <- [mName] ]
            , [ "body" .= encodedBody ]
            ]
    & withVisited visitedDefs globalId

exportRepl :: M ()
exportRepl =
    do
        repl <- Transaction.readIRef replIRef >>= ExprIRef.readVal & trans
        recurse repl
        Aeson.object
            [ "repl" .= encodeVal repl
            ] & tell

export :: T ViewM Aeson.Value
export = run exportRepl <&> snd
