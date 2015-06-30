{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , InTransaction(..)
    , NameConvertor, CPSNameConvertor
    , toDef, toExpression
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad ((<=<))
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (Traversable, traverse)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Sugar.Names.CPS (CPS(..))
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Types

type T = Transaction

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

newtype InTransaction m tm = InTransaction (forall a. m a -> T tm a)

-- TODO: Rename MonadNameWalk
class (MonadA m, MonadA (TM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type TM m :: * -> *
    opRun :: m (InTransaction m (TM m))

    opWithParamName :: NameGen.IsFunction -> CPSNameConvertor m
    opWithWhereItemName :: NameGen.IsFunction -> CPSNameConvertor m
    opWithDefName :: CPSNameConvertor m
    opWithTagName :: CPSNameConvertor m
    opGetDefName :: NameConvertor m
    opGetTagName :: NameConvertor m
    opGetTIdName :: NameConvertor m
    opGetParamName :: NameConvertor m
    opGetHiddenParamsName :: NameConvertor m

type OldExpression m a = Expression (OldName m) (TM m) a
type NewExpression m a = Expression (NewName m) (TM m) a

isFunctionType :: Type -> NameGen.IsFunction
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NotFunction

toTagG :: MonadNaming m => TagG (OldName m) -> m (TagG (NewName m))
toTagG = tagGName opGetTagName

toRecordField ::
    MonadNaming m =>
    RecordField (OldName m) (TM m) (OldExpression m a) ->
    m (RecordField (NewName m) (TM m) (NewExpression m a))
toRecordField recordField@RecordField {..} =
    do
        tag <- toTagG _rfTag
        expr <- toExpression _rfExpr
        pure recordField
            { _rfTag = tag
            , _rfExpr = expr
            }

toRecord ::
    MonadNaming m =>
    Record (OldName m) (TM m) (OldExpression m a) ->
    m (Record (NewName m) (TM m) (NewExpression m a))
toRecord record@Record {..} =
    do
        items <- traverse toRecordField _rItems
        t <- traverse toExpression _rTail
        pure record { _rItems = items, _rTail = t }

toCaseAlt ::
    MonadNaming m =>
    CaseAlt (OldName m) (TM m) (OldExpression m a) ->
    m (CaseAlt (NewName m) (TM m) (NewExpression m a))
toCaseAlt alt@CaseAlt {..} =
    do
        tag <- toTagG _caTag
        handler <- toExpression _caHandler
        pure alt
            { _caTag = tag
            , _caHandler = handler
            }

toCase ::
    MonadNaming m =>
    Case (OldName m) (TM m) (OldExpression m a) ->
    m (Case (NewName m) (TM m) (NewExpression m a))
toCase case_@Case {..} =
    do
        kind <- traverse toExpression _cKind
        alts <- traverse toCaseAlt _cAlts
        t <- traverse toExpression _cTail
        pure case_ { _cKind = kind, _cAlts = alts, _cTail = t }

toGetField ::
    MonadNaming m =>
    GetField (OldName m) (TM m) (OldExpression m a) ->
    m (GetField (NewName m) (TM m) (NewExpression m a))
toGetField getField@GetField {..} =
    do
        record <- toExpression _gfRecord
        tag <- toTagG _gfTag
        pure getField { _gfRecord = record, _gfTag = tag }

toInject ::
    MonadNaming m =>
    Inject (OldName m) (TM m) (OldExpression m a) ->
    m (Inject (NewName m) (TM m) (NewExpression m a))
toInject inject@Inject {..} =
    do
        val <- toExpression _iVal
        tag <- toTagG _iTag
        pure inject { _iVal = val, _iTag = tag }

toScopeGetVar ::
    MonadNaming m =>
    ScopeGetVar (OldName m) (TM m) ->
    m (ScopeGetVar (NewName m) (TM m))
toScopeGetVar (ScopeGetVar gv val) = (`ScopeGetVar` val) <$> toGetVar gv

toHoleResult ::
    MonadNaming m =>
    HoleResult (OldName m) (TM m) ->
    m (HoleResult (NewName m) (TM m))
toHoleResult = holeResultConverted toExpression

toHoleActions ::
    MonadNaming m =>
    HoleActions (OldName m) (TM m) ->
    m (HoleActions (NewName m) (TM m))
toHoleActions ha@HoleActions {..} =
    do
        InTransaction run <- opRun
        pure ha
            { _holeScope =
                run . traverse toScopeGetVar =<< _holeScope
            , _holeTIds =
                run . traverse toTIdG =<< _holeTIds
            , _holeResults =
                _holeResults
                & Lens.mapped . Lens.mapped . _2 %~
                    (>>= run . toHoleResult)
            }

toHoleArg ::
    MonadNaming m =>
    HoleArg (OldName m) (TM m) (OldExpression m a) ->
    m (HoleArg (NewName m) (TM m) (NewExpression m a))
toHoleArg arg@HoleArg{..} =
    do
        expr <- toExpression _haExpr
        getFieldTags <- mapM toTagG _haGetFieldTags
        pure arg
            { _haExpr = expr
            , _haGetFieldTags = getFieldTags
            }

toSuggested ::
    MonadNaming m =>
    HoleSuggested (OldName m) (TM m) ->
    m (HoleSuggested (NewName m) (TM m))
toSuggested suggested =
    do
        InTransaction run <- opRun
        suggested
            & hsSugaredBaseExpr %~ (>>= run . toExpression)
            & pure

toHole ::
    MonadNaming m =>
    Hole (OldName m) (TM m) (OldExpression m a) ->
    m (Hole (NewName m) (TM m) (NewExpression m a))
toHole hole@Hole {..} =
    do
        mActions <- _holeMActions & Lens._Just %%~ toHoleActions
        mArg <- _holeMArg & Lens._Just %%~ toHoleArg
        suggested <- _holeSuggesteds & Lens.traversed %%~ toSuggested
        pure hole
            { _holeMActions = mActions
            , _holeMArg = mArg
            , _holeSuggesteds = suggested
            }

toNamedVar ::
    MonadNaming m =>
    NamedVar (OldName m) (TM m) ->
    m (NamedVar (NewName m) (TM m))
toNamedVar namedVar =
    nvName f namedVar
    where
        f =
            case namedVar ^. nvVarType of
            GetParameter      -> opGetParamName
            GetFieldParameter -> opGetTagName
            GetDefinition     -> opGetDefName

toParamsRecordVar ::
    MonadNaming m => ParamsRecordVar (OldName m) ->
    m (ParamsRecordVar (NewName m))
toParamsRecordVar (ParamsRecordVar names) =
    ParamsRecordVar <$> traverse opGetTagName names

toGetVar ::
    MonadNaming m =>
    GetVar (OldName m) (TM m) ->
    m (GetVar (NewName m) (TM m))
toGetVar (GetVarNamed x) =
    GetVarNamed <$> toNamedVar x
toGetVar (GetVarParamsRecord x) =
    GetVarParamsRecord <$> toParamsRecordVar x

toTIdG :: MonadNaming m => TIdG (OldName m) -> m (TIdG (NewName m))
toTIdG = tidgName %%~ opGetTIdName

toNominal ::
    MonadNaming m =>
    Nominal (OldName m) (TM m) (OldExpression m a) ->
    m (Nominal (NewName m) (TM m) (NewExpression m a))
toNominal nom@Nominal {..} =
    do
        val <- toExpression _nVal
        tid <- toTIdG _nTId
        return nom { _nVal = val, _nTId = tid }

toApply ::
    MonadNaming m =>
    Apply (OldName m) (OldExpression m a) ->
    m (Apply (NewName m) (NewExpression m a))
toApply la@Apply{..} =
    do
        func <- toExpression _aFunc
        specialArgs <- traverse toExpression _aSpecialArgs
        annotatedArgs <- traverse (aaTag toTagG <=< aaExpr toExpression) _aAnnotatedArgs
        pure la
            { _aFunc = func
            , _aSpecialArgs = specialArgs
            , _aAnnotatedArgs = annotatedArgs
            }

traverseToExpr ::
    (MonadNaming m, Traversable t) =>
    (t (NewExpression m a) -> b) -> t (OldExpression m a) ->
    m b
traverseToExpr cons body = cons <$> traverse toExpression body

toBody ::
    MonadNaming m =>
    Body (OldName m) (TM m) (OldExpression m a) ->
    m (Body (NewName m) (TM m) (NewExpression m a))
toBody (BodyList x)           = traverseToExpr BodyList x
toBody (BodyLiteralInteger x) = pure $ BodyLiteralInteger x
--
toBody (BodyGetField x) = BodyGetField <$> toGetField x
toBody (BodyInject x) = BodyInject <$> toInject x
toBody (BodyRecord x) = BodyRecord <$> toRecord x
toBody (BodyCase x) = BodyCase <$> toCase x
toBody (BodyLam x) = BodyLam <$> toBinder x
toBody (BodyApply x) = BodyApply <$> toApply x
toBody (BodyHole x) = BodyHole <$> toHole x
toBody (BodyGetVar x) = BodyGetVar <$> toGetVar x
toBody (BodyToNom x) = BodyToNom <$> toNominal x
toBody (BodyFromNom x) = BodyFromNom <$> toNominal x

toExpression ::
    MonadNaming m => OldExpression m a ->
    m (NewExpression m a)
toExpression = rBody toBody

withWhereItem ::
    MonadNaming m =>
    WhereItem (OldName m) (TM m) (OldExpression m a) ->
    CPS m (WhereItem (NewName m) (TM m) (NewExpression m a))
withWhereItem item@WhereItem{..} =
    CPS $ \k -> do
        (name, (value, res)) <-
            runCPS (opWithWhereItemName (isFunctionType (_wiAnnotation ^. aInferredType)) _wiName) $
            (,) <$> toBinder _wiValue <*> k
        pure (item { _wiValue = value, _wiName = name }, res)

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (TM m) -> CPS m (BinderParams (NewName m) (TM m))
withBinderParams DefintionWithoutParams = pure DefintionWithoutParams
withBinderParams (NullParam a) = pure (NullParam a)
withBinderParams (VarParam FuncParam{..}) =
    opWithParamName (isFunctionType (_fpAnnotation ^. aInferredType)) _fpName
    <&> VarParam . \_fpName -> FuncParam{..}
withBinderParams (FieldParams xs) =
    (traverse . _2) f xs <&> FieldParams
    where
        f FuncParam{..} = opWithTagName _fpName <&> \_fpName -> FuncParam{..}

toBinder ::
    MonadNaming m =>
    Binder (OldName m) (TM m) (OldExpression m a) ->
    m (Binder (NewName m) (TM m) (NewExpression m a))
toBinder binder@Binder{..} =
    do
        (params, (whereItems, body)) <-
            runCPS (withBinderParams _bParams) .
            runCPS (traverse withWhereItem _bWhereItems) $
            toExpression _bBody
        binder
            { _bParams = params
            , _bBody = body
            , _bWhereItems = whereItems
            } & pure

toDefinitionBody ::
    MonadNaming m =>
    DefinitionBody (OldName m) (TM m) (OldExpression m a) ->
    m (DefinitionBody (NewName m) (TM m) (NewExpression m a))
toDefinitionBody (DefinitionBodyBuiltin bi) =
    pure (DefinitionBodyBuiltin bi)
toDefinitionBody
    (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
        DefinitionBodyExpression <$>
        (DefinitionExpression typeInfo <$> toBinder content)

toDef ::
    MonadNaming m =>
    Definition (OldName m) (TM m) (OldExpression m a) ->
    m (Definition (NewName m) (TM m) (NewExpression m a))
toDef def@Definition {..} =
    do
        (name, body) <- runCPS (opWithDefName _drName) $ toDefinitionBody _drBody
        pure def { _drName = name, _drBody = body }
