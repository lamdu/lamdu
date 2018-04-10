-- | Convert eval results

module Lamdu.Sugar.Convert.Eval
    ( convertEvalResults, convertEvalParam
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Except.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.Text.Encoding (decodeUtf8')
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

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
flattenRecord (ER.Val _ (ER.RRecExtend (V.RecExtend tag val rest))) =
    flattenRecord rest
    <&> _1 %~ ((tag, val) :)
    <&> _2 . Lens.at tag ?~ val
flattenRecord (ER.Val _ (ER.RError err)) = Left err
flattenRecord _ = Left (EvalTypeError "Record extents non-record")

mkTagInfo :: EntityId -> T.Tag -> TagInfo InternalName
mkTagInfo entityId tag =
    TagInfo
    { _tagName = nameWithoutContext tag
    , _tagInstance = EntityId.ofTag entityId tag
    , _tagVal = tag
    }

convertNullaryInject :: EntityId -> V.Inject (ER.Val pl) -> Maybe (ResBody InternalName a)
convertNullaryInject entityId (V.Inject tag (ER.Val _ ER.RRecEmpty)) =
    RInject (ResInject (mkTagInfo entityId tag) Nothing) & Just
convertNullaryInject _ _ = Nothing

convertStream ::
    Monad m =>
    EntityId -> T.Type -> V.Inject ERV -> MaybeT m (ResVal InternalName)
convertStream entityId typ (V.Inject _ val) =
    do
        T.TInst tid _ <- pure typ
        guard (tid == Builtins.streamTid)
        (_, fields) <- flattenRecord val & either (const Nothing) Just & maybeToMPlus
        hd <- fields ^? Lens.ix Builtins.headTag & maybeToMPlus
        ER.RFunc{} <- fields ^? Lens.ix Builtins.tailTag . ER.body & maybeToMPlus
        hdS <- convertVal (EntityId.ofEvalField Builtins.headTag entityId) hd & lift
        ResStream hdS & RStream & ResVal entityId & pure

convertInject ::
    Monad m => EntityId -> T.Type -> V.Inject ERV -> m (ResVal InternalName)
convertInject entityId typ inj =
    do
        convertNullaryInject entityId inj <&> ResVal entityId & maybeToMPlus & justToLeft
        convertStream entityId typ inj & justToLeft
        case inj of
            V.Inject tag val ->
                convertVal (EntityId.ofEvalField tag entityId) val <&> Just
                <&> ResInject (mkTagInfo entityId tag) <&> RInject
                <&> ResVal entityId & lift
    & runMatcherT

convertPlainRecord ::
    Monad m =>
    EntityId -> Either EvalTypeError [(T.Tag, ER.Val T.Type)] ->
    m (ResVal InternalName)
convertPlainRecord entityId (Left err) = RError err & ResVal entityId & pure
convertPlainRecord entityId (Right fields) =
    fields
    & traverse %%~ convertField
    <&> ResRecord <&> RRecord <&> ResVal entityId
    where
        convertField (tag, val) =
            convertVal (EntityId.ofEvalField tag entityId) val
            <&> (,) (mkTagInfo entityId tag)

convertTree ::
    Monad m =>
    EntityId -> T.Type -> Either e (Map T.Tag ERV) ->
    MaybeT m (ResVal InternalName)
convertTree entityId typ fr =
    do
        Right fields <- pure fr
        T.TInst tid _ <- pure typ
        guard (tid == Builtins.treeTid)
        root <- fields ^? Lens.ix Builtins.rootTag & maybeToMPlus
        subtrees <- fields ^? Lens.ix Builtins.subtreesTag & maybeToMPlus
        rootS <-
            convertVal (EntityId.ofEvalField Builtins.rootTag entityId) root
            & lift
        subtreesS <-
            subtrees ^.. ER.body . ER._RArray . Lens.folded
            & Lens.itraverse convertSubtree & lift
        ResTree rootS subtreesS & RTree & ResVal entityId & pure
    where
        convertSubtree idx =
            EntityId.ofEvalField Builtins.subtreesTag entityId
            & EntityId.ofEvalArrayIdx idx
            & convertVal

convertRecord ::
    Monad m => EntityId -> T.Type -> ERV -> m (ResVal InternalName)
convertRecord entityId typ v =
    do
        let fr = flattenRecord v
        convertTree entityId typ (fr <&> snd) & justToLeft
        lift (convertPlainRecord entityId (fr <&> fst))
    & runMatcherT

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

convertArray ::
    Monad m =>
    EntityId -> T.Type -> [ER.Val T.Type] -> m (ResVal InternalName)
convertArray entityId _typ vs =
    do
        vsS <- Lens.itraverse convertElem vs & lift
        convertRecordArray entityId vsS & maybeToMPlus & justToLeft
        RArray vsS & ResVal entityId & pure
    & runMatcherT
    where
        convertElem idx = convertVal (EntityId.ofEvalArrayIdx idx entityId)

convertVal :: Monad m => EntityId -> ERV -> m (ResVal InternalName)
convertVal entityId (ER.Val _ (ER.RError err)) = RError err & ResVal entityId & pure
convertVal entityId (ER.Val _ (ER.RFunc i)) = RFunc i & ResVal entityId & pure
convertVal entityId (ER.Val _ ER.RRecEmpty) = ResRecord [] & RRecord & ResVal entityId & pure
convertVal entityId v@(ER.Val typ ER.RRecExtend{}) = convertRecord entityId typ v
convertVal entityId (ER.Val typ (ER.RPrimVal p)) = convertPrimVal typ p & ResVal entityId & pure
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
    Monad m =>
    (ScopeId -> EntityId) -> EvalScopes ERV ->
    m (EvaluationScopes InternalName)
convertEvalResultsWith entityId evalResults =
    evalResults
    & Lens.traversed .> Lens.itraversed %%@~ convertVal . entityId
    <&> Lens.mapped %~ nullToNothing

convertEvalResults ::
    Monad m =>
    EntityId -> EvalScopes ERV -> m (EvaluationScopes InternalName)
convertEvalResults = convertEvalResultsWith . entityIdForEvalResult

-- | We flatten all the scopes the param received in ALL parent
-- scopes. The navigation is done via the lambda's scope map, and then
-- this map is used to just figure out the val of the param in some
-- (deeply) nested scope
convertEvalParam ::
    Monad m =>
    EntityId -> EvalScopes [(ScopeId, ERV)] ->
    m (EvaluationScopes InternalName)
convertEvalParam entityId evalResults =
    evalResults <&> (^.. Lens.folded . Lens.folded) <&> Map.fromList
    & convertEvalResultsWith (entityIdForParam entityId)
