-- | Convert eval results

module Lamdu.Sugar.Convert.Eval
    ( results, param, completion
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe.Extended (maybeToMPlus)
import           Data.Text.Encoding (decodeUtf8')
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
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

convertPrimVal :: T.Type -> V.PrimVal -> ResBody name a
convertPrimVal (T.TInst tid fields) p
    | Map.null fields
      && tid == Builtins.textTid =
        case PrimVal.toKnown p of
        PrimVal.Bytes x -> decodeUtf8' x & either (const (RBytes x)) RText
        _ -> RError (EvalTypeError "text not made of bytes")
convertPrimVal _ p =
    case PrimVal.toKnown p of
    PrimVal.Bytes x -> RBytes x
    PrimVal.Float x -> RFloat x

type ERV = ER.Val T.Type

flattenRecord :: ERV -> Either EvalTypeError ([(T.Tag, ERV)], Map T.Tag ERV)
flattenRecord (ER.Val _ ER.RRecEmpty) = Right ([], Map.empty)
flattenRecord (ER.Val _ (ER.RRecExtend (V.RecExtend tag v rest))) =
    flattenRecord rest
    <&> _1 %~ ((tag, v) :)
    <&> _2 . Lens.at tag ?~ v
flattenRecord (ER.Val _ (ER.RError err)) = Left err
flattenRecord _ = Left (EvalTypeError "Record extents non-record")

mkTagInfo :: EntityId -> T.Tag -> TagInfo InternalName
mkTagInfo entityId tag =
    TagInfo
    { _tagName = nameWithoutContext tag
    , _tagInstance = EntityId.ofTag entityId tag
    , _tagVal = tag
    }

convertNullaryInject :: EntityId -> V.Inject (ER.Val pl) -> Maybe (ResVal InternalName)
convertNullaryInject entityId (V.Inject tag (ER.Val _ ER.RRecEmpty)) =
    RInject (ResInject (mkTagInfo entityId tag) Nothing) & ResVal entityId & Just
convertNullaryInject _ _ = Nothing

convertStream :: EntityId -> T.Type -> V.Inject ERV -> Maybe (ResVal InternalName)
convertStream entityId typ (V.Inject _ x) =
    do
        T.TInst tid _ <- Just typ
        guard (tid == Builtins.streamTid)
        (_, fields) <- flattenRecord x & either (const Nothing) Just & maybeToMPlus
        hd <- fields ^? Lens.ix Builtins.headTag & maybeToMPlus
        ER.RFunc{} <- fields ^? Lens.ix Builtins.tailTag . ER.body & maybeToMPlus
        convertVal (EntityId.ofEvalField Builtins.headTag entityId) hd
            & ResStream & RStream & ResVal entityId & Just

simpleInject :: EntityId -> V.Inject (ER.Val T.Type) -> ResVal InternalName
simpleInject entityId (V.Inject tag x) =
    convertVal (EntityId.ofEvalField tag entityId) x
    & Just
    & ResInject (mkTagInfo entityId tag) & RInject
    & ResVal entityId

convertInject :: EntityId -> T.Type -> V.Inject ERV -> ResVal InternalName
convertInject entityId typ inj =
    convertNullaryInject entityId inj
    <|> convertStream entityId typ inj
    & fromMaybe (simpleInject entityId inj)

convertPlainRecord ::
    EntityId -> Either EvalTypeError [(T.Tag, ER.Val T.Type)] ->
    ResVal InternalName
convertPlainRecord entityId (Left err) = RError err & ResVal entityId
convertPlainRecord entityId (Right fields) =
    fields
    <&> convertField
    & ResRecord & RRecord & ResVal entityId
    where
        convertField (tag, x) =
            convertVal (EntityId.ofEvalField tag entityId) x
            & (,) (mkTagInfo entityId tag)

convertTree ::
    EntityId -> T.Type -> Either e (Map T.Tag ERV) ->
    Maybe (ResVal InternalName)
convertTree entityId typ fr =
    do
        Right fields <- Just fr
        T.TInst tid _ <- Just typ
        guard (tid == Builtins.treeTid)
        root <- fields ^? Lens.ix Builtins.rootTag
        subtrees <- fields ^? Lens.ix Builtins.subtreesTag
        RTree ResTree
            { _rtRoot = convertVal (EntityId.ofEvalField Builtins.rootTag entityId) root
            , _rtSubtrees =
                subtrees ^.. (ER.body . ER._RArray) .> Lens.folded
                & Lens.imapped %@~ convertSubtree
            } & ResVal entityId & Just
    where
        convertSubtree idx =
            EntityId.ofEvalField Builtins.subtreesTag entityId
            & EntityId.ofEvalArrayIdx idx
            & convertVal

convertRecord :: EntityId -> T.Type -> ERV -> ResVal InternalName
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

convertArray :: EntityId -> T.Type -> [ER.Val T.Type] -> ResVal InternalName
convertArray entityId _typ vs =
    convertRecordArray entityId vsS
    & fromMaybe (RArray vsS & ResVal entityId)
    where
        vsS = Lens.imap convertElem vs
        convertElem idx = convertVal (EntityId.ofEvalArrayIdx idx entityId)

convertVal :: EntityId -> ERV -> ResVal InternalName
convertVal entityId (ER.Val _ (ER.RError err)) = RError err & ResVal entityId
convertVal entityId (ER.Val _ (ER.RFunc i)) = RFunc i & ResVal entityId
convertVal entityId (ER.Val _ ER.RRecEmpty) = ResRecord [] & RRecord & ResVal entityId
convertVal entityId v@(ER.Val typ ER.RRecExtend{}) = convertRecord entityId typ v
convertVal entityId (ER.Val typ (ER.RPrimVal p)) = convertPrimVal typ p & ResVal entityId
convertVal entityId (ER.Val typ (ER.RInject x)) = convertInject entityId typ x
convertVal entityId (ER.Val typ (ER.RArray x)) = convertArray entityId typ x

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
    & Lens.mapped .> Lens.imapped %@~ fmap pure . convertVal . entityId
    <&> nullToNothing

results ::
    Applicative i =>
    EntityId -> EvalScopes ERV -> EvaluationScopes InternalName i
results = convertEvalResultsWith . entityIdForEvalResult

-- | We flatten all the scopes the param received in ALL parent
-- scopes. The navigation is done via the lambda's scope map, and then
-- this map is used to just figure out the val of the param in some
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
    EntityId -> CurAndPrev (Maybe (Either (ER.EvalException (ValI m)) ERV)) ->
    EvalCompletion InternalName (T m)
completion cp entityId completions =
    completions <&> Lens._Just %~ f
    where
        f (Left (ER.EvalException errType desc position)) =
                EvalError EvalException
                { _evalExceptionType = errType
                , _evalExceptionDesc = desc
                , _evalExceptionJumpTo =
                    position
                    <&>
                    \(whichGlobal, valI) ->
                    EntityId.ofValI valI
                    <$ case whichGlobal of
                    ER.GlobalRepl -> pure ()
                    ER.GlobalDef varId -> DataOps.newPane cp (ExprIRef.defI varId)
                }
        f (Right x) = convertVal entityId x & EvalSuccess
