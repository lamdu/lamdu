-- | Convert eval results

module Lamdu.Sugar.Convert.Eval
    ( results, param, completion
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Data.Char (chr)
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe.Extended (maybeToMPlus)
import           Data.Text.Encoding (decodeUtf8')
import           Data.UUID (UUID)
import           Hyper.Syntax.Nominal (NominalInst(..))
import           Hyper.Syntax.Row (RowExtend(..))
import           Hyper.Syntax.Scheme (QVarInstances(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

nullToNothing :: Map k v -> Maybe (Map k v)
nullToNothing m
    | Map.null m = Nothing
    | otherwise = Just m

convertPrimVal :: Pure # T.Type -> V.PrimVal -> ResBody name a
convertPrimVal (Pure (T.TInst (NominalInst tid (T.Types (QVarInstances tp) (QVarInstances rp))))) p
    | isSimpleBuiltin Builtins.textTid =
        case PrimVal.toKnown p of
        PrimVal.Bytes x -> decodeUtf8' x & either (const (RBytes x)) RText
        _ -> RError (EvalTypeError "text not made of bytes")
    | isSimpleBuiltin Builtins.charTid =
        case PrimVal.toKnown p of
        PrimVal.Float x
            | x /= fromInteger (truncate x)
            || x < 0
            || x >= (2 ^^ (32::Int)) -> RError (EvalTypeError "Char with invalid code point")
            | otherwise -> truncate x & chr & RChar
        _ -> RError (EvalTypeError "text not made of bytes")
    where
        isSimpleBuiltin t = Map.null tp && Map.null rp && tid == t
convertPrimVal _ p =
    case PrimVal.toKnown p of
    PrimVal.Bytes x -> RBytes x
    PrimVal.Float x -> RFloat x

type ERV = ER.Val (Pure # T.Type)

flattenRecord :: ERV -> Either EvalTypeError ([(T.Tag, ERV)], Map T.Tag ERV)
flattenRecord (Ann _ ER.RRecEmpty) = Right ([], Map.empty)
flattenRecord (Ann _ (ER.RRecExtend (RowExtend tag v rest))) =
    flattenRecord rest
    <&> _1 %~ ((tag, v) :)
    <&> _2 . Lens.at tag ?~ v
flattenRecord (Ann _ (ER.RError err)) = Left err
flattenRecord _ = Left (EvalTypeError "Record extents non-record")

convertNullaryInject :: EntityId -> ER.Inject # Ann a -> Maybe (ResVal InternalName)
convertNullaryInject entityId (ER.Inject tag (Ann _ ER.RRecEmpty)) =
    RInject (ResInject (ConvertTag.withoutContext entityId tag) Nothing)
    & ResVal entityId & Just
convertNullaryInject _ _ = Nothing

convertList :: EntityId -> Pure # T.Type -> ER.Inject # Annotated (Pure # T.Type) -> Maybe (ResVal InternalName)
convertList entityId typ (ER.Inject _ x) =
    do
        Pure (T.TInst (NominalInst tid _)) <- Just typ
        guard (tid == Builtins.listTid)
        (_, fields) <- flattenRecord x ^? Lens._Right & maybeToMPlus
        hd <- fields ^? Lens.ix Builtins.headTag & maybeToMPlus
        ER.RFunc{} <- fields ^? Lens.ix Builtins.tailTag . hVal & maybeToMPlus
        convertVal (EntityId.ofEvalField Builtins.headTag entityId) hd
            & ResList & RList & ResVal entityId & Just

simpleInject :: EntityId -> ER.Inject # Annotated (Pure # T.Type) -> ResVal InternalName
simpleInject entityId (ER.Inject tag x) =
    convertVal (EntityId.ofEvalField tag entityId) x
    & Just
    & ResInject (ConvertTag.withoutContext entityId tag) & RInject
    & ResVal entityId

convertInject :: EntityId -> Pure # T.Type -> ER.Inject # Annotated (Pure # T.Type) -> ResVal InternalName
convertInject entityId typ inj =
    convertNullaryInject entityId inj
    <|> convertList entityId typ inj
    & fromMaybe (simpleInject entityId inj)

convertPlainRecord ::
    EntityId -> Either EvalTypeError [(T.Tag, ERV)] ->
    ResVal InternalName
convertPlainRecord entityId (Left err) = RError err & ResVal entityId
convertPlainRecord entityId (Right fields) =
    fields
    <&> convertField
    & ResRecord & RRecord & ResVal entityId
    where
        convertField (tag, x) =
            convertVal (EntityId.ofEvalField tag entityId) x
            & (,) (ConvertTag.withoutContext entityId tag)

convertTree ::
    EntityId -> Pure # T.Type -> Either e (Map T.Tag ERV) ->
    Maybe (ResVal InternalName)
convertTree entityId typ fr =
    do
        Right fields <- Just fr
        Pure (T.TInst (NominalInst tid _)) <- Just typ
        guard (tid == Builtins.treeTid)
        root <- fields ^? Lens.ix Builtins.rootTag
        subtrees <- fields ^? Lens.ix Builtins.subtreesTag
        RTree ResTree
            { _rtRoot = convertVal (EntityId.ofEvalField Builtins.rootTag entityId) root
            , _rtSubtrees =
                subtrees ^.. (hVal . ER._RArray) .> Lens.folded
                & Lens.imapped %@~ convertSubtree
            } & ResVal entityId & Just
    where
        convertSubtree idx =
            EntityId.ofEvalField Builtins.subtreesTag entityId
            & EntityId.ofEvalArrayIdx idx
            & convertVal

convertRecord :: EntityId -> Pure # T.Type -> ERV -> ResVal InternalName
convertRecord entityId typ v =
    convertTree entityId typ (fr <&> snd)
    & fromMaybe (convertPlainRecord entityId (fr <&> fst))
    where
        fr = flattenRecord v

-- | Array of records -> Record of arrays
convertRecordArray :: EntityId -> [ResVal name] -> Maybe (ResVal name)
convertRecordArray entityId rows =
    do
        -- at least 2 rows:
        Lens.has (Lens.ix 1) rows & guard
        -- all eval to a record:
        recordRows <- traverse (^? resBody . _RRecord) rows <&> map (^. recordFields)
        -- get the record tags (# columns)
        tags <- recordRows ^? Lens.ix 0 <&> map fst
        -- At least 1 column should exist
        Lens.has (Lens.ix 0) tags & guard
        let tagVals = tags <&> (^. tagVal)
        let taggedRecordRows = recordRows <&> Lens.mapped . _1 %~ (^. tagVal)
        ResTable
            { _rtHeaders = tags
            , _rtRows = taggedRecordRows <&> toRow tagVals
            } & RTable & ResVal entityId & Just
    where
        toRow tags rowFields
           | length tags /= length rowFields =
               error "convertRecordArray: tags mismatch"
           | otherwise =
               traverse (`List.lookup` rowFields) tags
               & fromMaybe (error "makeArray: tags mismatch")

convertArray :: EntityId -> Pure # T.Type -> [ERV] -> ResVal InternalName
convertArray entityId _typ vs =
    convertRecordArray entityId vsS
    & fromMaybe (RArray vsS & ResVal entityId)
    where
        vsS = Lens.imap convertElem vs
        convertElem idx = convertVal (EntityId.ofEvalArrayIdx idx entityId)

convertVal :: EntityId -> ERV -> ResVal InternalName
convertVal entityId (Ann _ (ER.RError err)) = RError err & ResVal entityId
convertVal entityId (Ann _ (ER.RFunc i)) = RFunc i & ResVal entityId
convertVal entityId (Ann _ ER.RRecEmpty) = ResRecord [] & RRecord & ResVal entityId
convertVal entityId v@(Ann (Const typ) ER.RRecExtend{}) = convertRecord entityId typ v
convertVal entityId (Ann (Const typ) (ER.RPrimVal p)) = convertPrimVal typ p & ResVal entityId
convertVal entityId (Ann (Const typ) (ER.RInject x)) = convertInject entityId typ x
convertVal entityId (Ann (Const typ) (ER.RArray x)) = convertArray entityId typ x

-- When we can scroll between eval view results we
-- must encode the scope into the entityID for smooth
-- scroll to work.
-- When we cannot, we'd rather not animate changes
-- within a scrolled scope (use same entityId).
entityIdForEvalResult :: EntityId -> ScopeId -> EntityId
entityIdForEvalResult entityId _ = entityId

entityIdForParam :: EntityId -> ScopeId -> EntityId
entityIdForParam entityId (ER.ScopeId scopeId) =
    EntityId.ofEvalArrayIdx scopeId entityId

convertEvalResultsWith ::
    Applicative i =>
    (ScopeId -> EntityId) -> EvalScopes ERV ->
    EvaluationScopes InternalName i
convertEvalResultsWith entityId evalResults =
    evalResults
    <&> Lens.imapped %@~ fmap pure . convertVal . entityId
    <&> nullToNothing

results ::
    Applicative i =>
    EntityId -> EvalScopes ERV -> EvaluationScopes InternalName i
results = convertEvalResultsWith . entityIdForEvalResult

-- | We flatten all the scopes the param received in ALL parent
-- scopes. The navigation is done via the lambda's scope map, and then
-- this map is used to just figure out the hVal of the param in some
-- (deeply) nested scope
param ::
    Applicative i =>
    EntityId -> EvalScopes [(ScopeId, ERV)] -> EvaluationScopes InternalName i
param entityId evalResults =
    evalResults <&> (^.. Lens.folded . Lens.folded) <&> Map.fromList
    & convertEvalResultsWith (entityIdForParam entityId)

completion ::
    Monad m =>
    Anchors.CodeAnchors m ->
    CurAndPrev (Maybe (Either (ER.EvalException UUID) a)) ->
    EvalCompletion (T m)
completion cp completions =
    completions <&> Lens._Just %~ f
    where
        f (Left (ER.EvalException errType position)) =
                EvalError EvalException
                { _evalExceptionType = errType
                , _evalExceptionJumpTo =
                    position
                    <&>
                    \(whichGlobal, valI) ->
                    EntityId.EntityId valI
                    <$ case whichGlobal of
                    ER.GlobalRepl -> pure ()
                    ER.GlobalDef varId ->
                        ExprIRef.defI varId
                        & Anchors.PaneDefinition
                        & DataOps.newPane cp
                }
        f Right{} = EvalSuccess
