{-# LANGUAGE LambdaCase, NoImplicitPrelude, FlexibleContexts, TypeFamilies, RecordWildCards, NamedFieldPuns #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), FunctionSignature(..)
    , NameConvertor, CPSNameConvertor
    , OldExpression, NewExpression
    , toWorkArea, toDef, toExpression, toBody
    ) where

import qualified Data.Set as Set
import           Data.Store.Transaction (Transaction)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Names.CPS (CPS(..))
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

data NameType = DefName | TagName | NominalName | ParamName
    deriving (Eq, Ord, Show)

data FunctionSignature = FunctionSignature
    { sSpecialArgs :: SpecialArgs ()
    , sNormalArgs :: Set T.Tag
    } deriving (Eq, Ord, Show)

-- TODO: Rename MonadNameWalk
class (Monad m, Monad (TM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type TM m :: * -> *
    opRun :: m (m a -> T (TM m) a)

    opWithParamName :: NameGen.VarInfo -> CPSNameConvertor m
    opWithLetName :: NameGen.VarInfo -> CPSNameConvertor m
    opWithTagName :: CPSNameConvertor m
    opGetName :: NameType -> NameConvertor m

    opGetAppliedFuncName :: FunctionSignature -> NameType -> NameConvertor m
    opGetAppliedFuncName _ = opGetName

type OldExpression m a = Expression (OldName m) (TM m) a
type NewExpression m a = Expression (NewName m) (TM m) a

isFunctionType :: Type -> NameGen.VarInfo
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NormalVar

toHoleResult ::
    MonadNaming m =>
    HoleResult (OldName m) (TM m) ->
    m (HoleResult (NewName m) (TM m))
toHoleResult = holeResultConverted toExpression

toHoleOption ::
    MonadNaming m =>
    HoleOption (OldName m) (TM m) ->
    m (HoleOption (NewName m) (TM m))
toHoleOption option@HoleOption{..} =
    do
        run0 <- opRun
        run1 <- opRun
        pure option
            { _hoSugaredBaseExpr = _hoSugaredBaseExpr >>= run0 . toExpression
            , _hoResults = _hoResults <&> second (>>= run1 . toHoleResult)
            }
    where
        {-# INLINE second #-}
        second f (x, y) = (x, f y)

toHoleActions ::
    MonadNaming m =>
    HoleActions (OldName m) (TM m) ->
    m (HoleActions (NewName m) (TM m))
toHoleActions ha@HoleActions {..} =
    do
        run <- opRun
        pure ha { _holeOptions = _holeOptions >>= run . traverse toHoleOption }

toParam ::
    MonadNaming m =>
    Param (OldName m) p ->
    m (Param (NewName m) p)
toParam param =
    (pNameRef . nrName) f param
    where
        f = case param ^. pForm of
            GetParameter      -> ParamName
            GetFieldParameter -> TagName
            & opGetName

binderVarType :: BinderVarForm t -> NameType
binderVarType GetLet = ParamName
binderVarType (GetDefinition _) = DefName

toBinderVar ::
    MonadNaming m =>
    BinderVar (OldName m) p ->
    m (BinderVar (NewName m) p)
toBinderVar binderVar =
    (bvNameRef . nrName) (opGetName (binderVarType (binderVar ^. bvForm))) binderVar

toGetVar ::
    MonadNaming m =>
    GetVar (OldName m) p ->
    m (GetVar (NewName m) p)
toGetVar (GetParam x) = toParam x <&> GetParam
toGetVar (GetBinder x) = toBinderVar x <&> GetBinder
toGetVar (GetParamsRecord x) = traverse (opGetName TagName) x <&> GetParamsRecord

toLet ::
    MonadNaming m => (a -> m b) ->
    Let (OldName m) p a ->
    m (Let (NewName m) p b)
toLet expr item@Let{..} =
    do
        (name, body) <-
            runCPS (opWithLetName (isFunctionType (_lAnnotation ^. aInferredType)) _lName) $
            toBinderBody expr _lBody
        value <- toBinder expr _lValue
        item { _lValue = value, _lName = name, _lBody = body } & pure

toBinderContent ::
    MonadNaming m => (a -> m b) ->
    BinderContent (OldName m) p a ->
    m (BinderContent (NewName m) p b)
toBinderContent expr (BinderLet l) = toLet expr l <&> BinderLet
toBinderContent expr (BinderExpr e) = expr e <&> BinderExpr

toBinderBody ::
    MonadNaming m => (a -> m b) ->
    BinderBody (OldName m) p a ->
    m (BinderBody (NewName m) p b)
toBinderBody expr = bbContent %%~ toBinderContent expr

toBinder ::
    MonadNaming m => (a -> m b) ->
    Binder (OldName m) p a ->
    m (Binder (NewName m) p b)
toBinder expr binder@Binder{..} =
    do
        (params, body) <-
            runCPS (withBinderParams _bParams) $ toBinderBody expr _bBody
        binder
            { _bParams = params
            , _bBody = body
            } & pure

toLam ::
    MonadNaming m => (a -> m b) ->
    Lambda (OldName m) p a ->
    m (Lambda (NewName m) p b)
toLam = lamBinder . toBinder

toTag ::
    MonadNaming m =>
    Tag (OldName m) p ->
    m (Tag (NewName m) p)
toTag = tagName %%~ opGetName TagName

toLabeledApply ::
    MonadNaming m =>
    (a -> m b) ->
    LabeledApply (OldName m) p a ->
    m (LabeledApply (NewName m) p b)
toLabeledApply expr app@LabeledApply{..} =
    LabeledApply
    <$>
    ( _aFunc & bvNameRef . nrName %%~
        opGetAppliedFuncName (funcSignature app) (binderVarType (_aFunc ^. bvForm))
    )
    <*> pure _aSpecialArgs
    <*> (traverse . aaName) (opGetName TagName) _aAnnotatedArgs
    <*> (traverse . raValue) toParam _aRelayedArgs
    >>= traverse expr

toLeafHoleActions ::
    MonadNaming m =>
    LeafHoleActions (OldName m) (TM m) -> m (LeafHoleActions (NewName m) (TM m))
toLeafHoleActions ha@LeafHoleActions {..} =
    do
        run <- opRun
        pure ha { _holeOptionLiteral = _holeOptionLiteral <&> (>>= run . toHoleOption) }

toHoleKind ::
    MonadNaming m =>
    (a -> m b) -> HoleKind (OldName m) (TM m) a -> m (HoleKind (NewName m) (TM m) b)
toHoleKind expr (WrapperHole holeArg) = traverse expr holeArg <&> WrapperHole
toHoleKind _ (LeafHole actions) = toLeafHoleActions actions <&> LeafHole

toHole ::
    MonadNaming m =>
    (a -> m b) -> Hole (OldName m) (TM m) a -> m (Hole (NewName m) (TM m) b)
toHole expr Hole{..} =
    Hole
    <$> toHoleActions _holeActions
    <*> toHoleKind expr _holeKind

toBody ::
    MonadNaming m => (a -> m b) ->
    Body (OldName m) (TM m) a ->
    m (Body (NewName m) (TM m) b)
toBody expr = \case
    BodyGetField     x -> x & traverse expr >>= gfTag toTag <&> BodyGetField
    BodyInject       x -> x & traverse expr >>= iTag toTag <&> BodyInject
    BodyRecord       x -> x & traverse expr >>= (rItems . traverse . ciTag) toTag <&> BodyRecord
    BodyCase         x -> x & traverse expr >>= (cAlts . traverse . ciTag) toTag <&> BodyCase
    BodyGuard        x -> x & traverse expr <&> BodyGuard
    BodySimpleApply  x -> x & traverse expr <&> BodySimpleApply
    BodyLabeledApply x -> x & toLabeledApply expr <&> BodyLabeledApply
    BodyHole         x -> x & toHole expr <&> BodyHole
    BodyFromNom      x -> x & traverse expr >>= nTId toTId <&> BodyFromNom
    BodyToNom        x -> x & traverse (toBinderBody expr) >>= nTId toTId <&> BodyToNom
    BodyGetVar       x -> x & toGetVar <&> BodyGetVar
    BodyLiteral      x -> x & BodyLiteral & pure
    BodyLam          x -> x & toLam expr <&> BodyLam
    BodyInjectedExpression -> return BodyInjectedExpression
    where
        toTId = tidName %%~ opGetName NominalName

funcSignature :: LabeledApply name binderVar a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sSpecialArgs = apply ^. aSpecialArgs & void
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression :: MonadNaming m => OldExpression m a -> m (NewExpression m a)
toExpression = rBody (toBody toExpression)

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) p ->
    CPS m (BinderParams (NewName m) p)
withBinderParams BinderWithoutParams = pure BinderWithoutParams
withBinderParams (NullParam a) = pure (NullParam a)
withBinderParams (VarParam fp) =
    opWithParamName (isFunctionType (fp ^. fpAnnotation . aInferredType))
    (fp ^. fpInfo . vpiName)
    <&> VarParam . \newName -> fp & fpInfo . vpiName .~ newName
withBinderParams (FieldParams xs) = (traverse . fpInfo . fpiTag . tagName) opWithTagName xs <&> FieldParams

toDefinitionBody ::
    MonadNaming m => (a -> m b) ->
    DefinitionBody (OldName m) p a ->
    m (DefinitionBody (NewName m) p b)
toDefinitionBody _ (DefinitionBodyBuiltin bi) = pure (DefinitionBodyBuiltin bi)
toDefinitionBody f (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
     toBinder f content
     <&> DefinitionExpression typeInfo
     <&> DefinitionBodyExpression

toDef ::
    MonadNaming m => (a -> m b) ->
    Definition (OldName m) p a ->
    m (Definition (NewName m) p b)
toDef f def@Definition {..} =
    do
        name <- opGetName DefName _drName
        body <- toDefinitionBody f _drBody
        pure def { _drName = name, _drBody = body }

toPane ::
    MonadNaming m =>
    Pane (OldName m) (TM m) a ->
    m (Pane (NewName m) (TM m) a)
toPane = paneDefinition (toDef toExpression)

toWorkArea ::
    MonadNaming m =>
    WorkArea (OldName m) (TM m) a ->
    m (WorkArea (NewName m) (TM m) a)
toWorkArea WorkArea { _waPanes, _waRepl } =
    WorkArea
    <$> traverse toPane _waPanes
    <*> toExpression _waRepl
