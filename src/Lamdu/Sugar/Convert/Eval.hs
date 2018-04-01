-- | Convert eval results

module Lamdu.Sugar.Convert.Eval
    ( convertEvalResults, convertEvalParam
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Except.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.CurAndPrev (CurAndPrev(..))
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
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

nullToNothing :: Map k v -> Maybe (Map k v)
nullToNothing m
    | Map.null m = Nothing
    | otherwise = Just m

convertPrimVal :: T.Type -> V.PrimVal -> ResBody a
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

flattenRecord :: ERV -> Either EvalError ([(T.Tag, ERV)], Map T.Tag ERV)
flattenRecord (ER.Val _ ER.RRecEmpty) = Right ([], Map.empty)
flattenRecord (ER.Val _ (ER.RRecExtend (V.RecExtend tag val rest))) =
    flattenRecord rest
    <&> _1 %~ ((tag, val) :)
    <&> _2 . Lens.at tag ?~ val
flattenRecord (ER.Val _ (ER.RError err)) = Left err
flattenRecord _ = Left (EvalTypeError "Record extents non-record")

convertNullaryInject :: V.Inject (ER.Val pl) -> Maybe (ResBody a)
convertNullaryInject (V.Inject tag (ER.Val _ ER.RRecEmpty)) =
    RInject (ResInject tag Nothing) & Just
convertNullaryInject _ = Nothing

convertStream :: Monad m => T.Type -> V.Inject ERV -> MaybeT (ConvertM m) ResVal
convertStream typ (V.Inject _ val) =
    do
        T.TInst tid _ <- pure typ
        guard (tid == Builtins.streamTid)
        (_, fields) <- flattenRecord val & either (const Nothing) Just & maybeToMPlus
        hd <- fields ^? Lens.ix Builtins.headTag & maybeToMPlus
        ER.RFunc{} <- fields ^? Lens.ix Builtins.tailTag . ER.body & maybeToMPlus
        hdS <- convertVal hd & lift
        ResStream hdS & RStream & ResVal & pure

convertInject :: Monad m => T.Type -> V.Inject ERV -> ConvertM m ResVal
convertInject typ inj =
    do
        convertNullaryInject inj <&> ResVal & maybeToMPlus & justToLeft
        convertStream typ inj & justToLeft
        case inj of
            V.Inject tag val ->
                lift (convertVal val) <&> Just <&> ResInject tag <&> RInject
                <&> ResVal
    & runMatcherT

convertPlainRecord ::
    Monad m => Either EvalError [(T.Tag, ER.Val T.Type)] -> ConvertM m ResVal
convertPlainRecord (Left err) = RError err & ResVal & pure
convertPlainRecord (Right fields) =
    fields & traverse . _2 %%~ convertVal <&> ResRecord <&> RRecord <&> ResVal

convertTree ::
    Monad m => T.Type -> Either e (Map T.Tag ERV) -> MaybeT (ConvertM m) ResVal
convertTree typ fr =
    do
        Right fields <- pure fr
        T.TInst tid _ <- pure typ
        guard (tid == Builtins.treeTid)
        root <- fields ^? Lens.ix Builtins.rootTag & maybeToMPlus
        subtrees <- fields ^? Lens.ix Builtins.subtreesTag & maybeToMPlus
        rootS <- convertVal root & lift
        subtreesS <-
            subtrees ^.. ER.body . ER._RArray . Lens.folded
            & traverse convertVal & lift
        ResTree rootS subtreesS & RTree & ResVal & pure

convertRecord :: Monad m => T.Type -> ERV -> ConvertM m ResVal
convertRecord typ v =
    do
        let fr = flattenRecord v
        convertTree typ (fr <&> snd) & justToLeft
        lift (convertPlainRecord (fr <&> fst))
    & runMatcherT

convertRecordArray :: [ResVal] -> Maybe ResVal
convertRecordArray rows =
    do
        -- at least 2 rows:
        Lens.has (Lens.ix 1) rows & guard
        -- all eval to a record:
        recordRows <- traverse (^? resBody . _RRecord) rows <&> map (^. recordFields)
        -- get the record tags (# columns)
        tags <- recordRows ^? Lens.ix 0 <&> map fst
        -- At least 1 column should exist
        Lens.has (Lens.ix 0) tags & guard
        ResTable
            { _rtHeaders = tags
            , _rtRows = recordRows <&> toRow tags
            } & RTable & ResVal & Just
    where
        toRow tags rowFields
           | length tags /= length rowFields = error "convertRecordArray: tags mismatch"
           | otherwise =
                 traverse (`List.lookup` rowFields) tags
                 & fromMaybe (error "makeArray: tags mismatch")

convertArray :: Monad m => T.Type -> [ER.Val T.Type] -> ConvertM m ResVal
convertArray _typ vs =
    do
        vsS <- traverse convertVal vs & lift
        convertRecordArray vsS & maybeToMPlus & justToLeft
        RArray vsS & ResVal & pure
    & runMatcherT

convertVal :: Monad m => ERV -> ConvertM m ResVal
convertVal (ER.Val _ (ER.RError err)) = RError err & ResVal & pure
convertVal (ER.Val _ (ER.RFunc i)) = RFunc i & ResVal & pure
convertVal (ER.Val _ ER.RRecEmpty) = ResRecord [] & RRecord & ResVal & pure
convertVal v@(ER.Val typ ER.RRecExtend{}) = convertRecord typ v
convertVal (ER.Val typ (ER.RPrimVal p)) = convertPrimVal typ p & ResVal & pure
convertVal (ER.Val typ (ER.RInject x)) = convertInject typ x
convertVal (ER.Val typ (ER.RArray x)) = convertArray typ x

convertEvalResults ::
    Monad m => CurAndPrev (Map ScopeId ERV) -> ConvertM m EvaluationScopes
convertEvalResults evalResults =
    evalResults
    & traverse . traverse %%~ convertVal
    <&> Lens.mapped %~ nullToNothing

-- | We flatten all the scopes the param received in ALL parent
-- scopes. The navigation is done via the lambda's scope map, and then
-- this map is used to just figure out the val of the param in some
-- (deeply) nested scope
convertEvalParam ::
    Monad m => CurAndPrev (Map ScopeId [(ScopeId, ERV)]) ->
    ConvertM m EvaluationScopes
convertEvalParam evalResults =
    evalResults <&> (^.. Lens.folded . Lens.folded) <&> Map.fromList
    & convertEvalResults
