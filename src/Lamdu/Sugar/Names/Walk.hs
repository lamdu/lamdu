{-# LANGUAGE LambdaCase, NoImplicitPrelude, FlexibleContexts, TypeFamilies, RecordWildCards, NamedFieldPuns #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), FunctionSignature(..), Disambiguator
    , NameConvertor, CPSNameConvertor
    , OldExpression, NewExpression
    , toWorkArea, toDef, toExpression, toBody
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Data.Store.Transaction (Transaction)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

data NameType = DefName | TagName | NominalName | ParamName | FieldParamName
    deriving (Eq, Ord, Show)

data FunctionSignature = FunctionSignature
    { sSpecialArgs :: SpecialArgs ()
    , sNormalArgs :: Set T.Tag
    } deriving (Eq, Ord, Show)

type Disambiguator = FunctionSignature

-- TODO: Rename MonadNameWalk
class (Monad m, Monad (SM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type SM m :: * -> *
    opRun :: m (m a -> T (SM m) a)

    opWithParamName :: ParameterForm -> NameGen.VarInfo -> CPSNameConvertor m
    opWithLetName :: NameGen.VarInfo -> CPSNameConvertor m
    opGetName :: Maybe Disambiguator -> NameType -> NameConvertor m

type TM m = T (SM m)

type OldExpression m a = Expression (OldName m) (TM m) a
type NewExpression m a = Expression (NewName m) (TM m) a

isFunctionType :: Type -> NameGen.VarInfo
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NormalVar

toHoleResult ::
    MonadNaming m =>
    HoleResult (TM m) (Expression (OldName m) (TM m) ()) ->
    m (HoleResult (TM m) (Expression (NewName m) (TM m) ()))
toHoleResult = holeResultConverted toExpression

toHoleOption ::
    MonadNaming m =>
    HoleOption (TM m) (Expression (OldName m) (TM m) ()) ->
    m (HoleOption (TM m) (Expression (NewName m) (TM m) ()))
toHoleOption option@HoleOption{..} =
    do
        run0 <- opRun
        run1 <- opRun
        pure option
            { _hoSugaredBaseExpr = _hoSugaredBaseExpr >>= run0 . toExpression
            , _hoResults = _hoResults <&> _2 %~ (>>= run1 . toHoleResult)
            }

toParamRef ::
    MonadNaming m =>
    ParamRef (OldName m) p ->
    m (ParamRef (NewName m) p)
toParamRef param =
    (pNameRef . nrName) f param
    where
        f = case param ^. pForm of
            GetParameter      -> ParamName
            GetFieldParameter -> FieldParamName
            & opGetName Nothing

binderVarType :: BinderVarForm t -> NameType
binderVarType GetLet = ParamName
binderVarType (GetDefinition _) = DefName

toBinderVarRef ::
    MonadNaming m =>
    BinderVarRef (OldName m) p ->
    m (BinderVarRef (NewName m) p)
toBinderVarRef binderVar =
    (bvNameRef . nrName)
    (opGetName Nothing (binderVarType (binderVar ^. bvForm))) binderVar

toGetVar ::
    MonadNaming m =>
    GetVar (OldName m) p ->
    m (GetVar (NewName m) p)
toGetVar (GetParam x) = toParamRef x <&> GetParam
toGetVar (GetBinder x) = toBinderVarRef x <&> GetBinder
toGetVar (GetParamsRecord x) =
    traverse (opGetName Nothing TagName) x <&> GetParamsRecord

toLet ::
    MonadNaming m => (a -> m b) ->
    Let (OldName m) (TM m) a ->
    m (Let (NewName m) (TM m) b)
toLet expr item@Let{..} =
    do
        (name, body) <-
            runCPS (opWithLetName (isFunctionType (_lAnnotation ^. aInferredType)) _lName) $
            toBinderBody expr _lBody
        value <- toBinder expr _lValue
        item { _lValue = value, _lName = name, _lBody = body } & pure

toBinderContent ::
    MonadNaming m => (a -> m b) ->
    BinderContent (OldName m) (TM m) a ->
    m (BinderContent (NewName m) (TM m) b)
toBinderContent expr (BinderLet l) = toLet expr l <&> BinderLet
toBinderContent expr (BinderExpr e) = expr e <&> BinderExpr

toBinderBody ::
    MonadNaming m => (a -> m b) ->
    BinderBody (OldName m) (TM m) a ->
    m (BinderBody (NewName m) (TM m) b)
toBinderBody expr = bbContent %%~ toBinderContent expr

toBinder ::
    MonadNaming m => (a -> m b) ->
    Binder (OldName m) (TM m) a ->
    m (Binder (NewName m) (TM m) b)
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
    Lambda (OldName m) (TM m) a ->
    m (Lambda (NewName m) (TM m) b)
toLam = lamBinder . toBinder

toTagSelection ::
    MonadNaming m =>
    TagSelection (OldName m) (TM m) a -> m (TagSelection (NewName m) (TM m) a)
toTagSelection TagSelection{..} =
    do
        run0 <- opRun
        run1 <- opRun
        pure TagSelection
            { _tsOptions =
                _tsOptions >>= run0 . (traverse . toName) (opGetName Nothing TagName)
            , _tsNewTag = _tsNewTag >>= run1 . _1 (opGetName Nothing TagName)
            }

toTag ::
    MonadNaming m =>
    Tag (OldName m) (TM m) ->
    m (Tag (NewName m) (TM m))
toTag (Tag info name actions) =
    Tag info
    <$> opGetName Nothing TagName name
    <*> toTagSelection actions

toLabeledApply ::
    MonadNaming m =>
    (a -> m b) ->
    LabeledApply (OldName m) p a ->
    m (LabeledApply (NewName m) p b)
toLabeledApply expr app@LabeledApply{..} =
    LabeledApply
    <$>
    ( _aFunc & bvNameRef . nrName %%~
        opGetName (Just (funcSignature app)) (binderVarType (_aFunc ^. bvForm))
    )
    <*> pure _aSpecialArgs
    <*> (traverse . aaName) (opGetName Nothing TagName) _aAnnotatedArgs
    <*> (traverse . raValue) toParamRef _aRelayedArgs
    >>= traverse expr

toHole ::
    MonadNaming m =>
    Hole (TM m) (Expression (OldName m) (TM m) ()) ->
    m (Hole (TM m) (Expression (NewName m) (TM m) ()))
toHole Hole{..} =
    do
        run0 <- opRun
        run1 <- opRun
        pure Hole
            { _holeOptions = _holeOptions >>= run0 . traverse toHoleOption
            , _holeOptionLiteral = _holeOptionLiteral <&> Lens.mapped . _2 %~ (>>= run1 . toHoleResult)
            , _holeMDelete = _holeMDelete
            }

toFragment ::
    MonadNaming m =>
    (a -> m b) ->
    Fragment (OldName m) (TM m) a ->
    m (Fragment (NewName m) (TM m) b)
toFragment expr Fragment{..} =
    do
        run <- opRun
        newExpr <- expr _fExpr
        pure Fragment
            { _fExpr = newExpr
            , _fAttach = _fAttach
            , _fOptions = _fOptions >>= run . traverse toHoleOption
            }

toComposite ::
    MonadNaming m =>
    (a -> m b) ->
    Composite (OldName m) (TM m) a ->
    m (Composite (NewName m) (TM m) b)
toComposite expr Composite{..} =
    (\_cItems _cAddItemWithTag -> Composite{..})
    <$> (traverse . ciTag) toTag _cItems
    <*> toTagSelection _cAddItemWithTag
    >>= traverse expr

toCase ::
    MonadNaming m =>
    (a -> m b) ->
    Case (OldName m) (TM m) a ->
    m (Case (NewName m) (TM m) b)
toCase expr (Case k c) = Case <$> traverse expr k <*> toComposite expr c

toBody ::
    MonadNaming m => (a -> m b) ->
    Body (OldName m) (TM m) a ->
    m (Body (NewName m) (TM m) b)
toBody expr = \case
    BodyGetField     x -> x & traverse expr >>= gfTag toTag <&> BodyGetField
    BodyInject       x -> x & traverse expr >>= iTag toTag <&> BodyInject
    BodyRecord       x -> x & toComposite expr <&> BodyRecord
    BodyCase         x -> x & toCase expr <&> BodyCase
    BodyIfElse       x -> x & traverse expr <&> BodyIfElse
    BodySimpleApply  x -> x & traverse expr <&> BodySimpleApply
    BodyLabeledApply x -> x & toLabeledApply expr <&> BodyLabeledApply
    BodyHole         x -> x & toHole <&> BodyHole
    BodyFromNom      x -> x & traverse expr >>= nTId toTId <&> BodyFromNom
    BodyToNom        x -> x & traverse (toBinderBody expr) >>= nTId toTId <&> BodyToNom
    BodyGetVar       x -> x & toGetVar <&> BodyGetVar
    BodyLiteral      x -> x & BodyLiteral & pure
    BodyLam          x -> x & toLam expr <&> BodyLam
    BodyFragment     x -> x & toFragment expr <&> BodyFragment
    BodyPlaceHolder    -> pure BodyPlaceHolder
    where
        toTId = tidName %%~ opGetName Nothing NominalName

funcSignature :: LabeledApply name binderVar a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sSpecialArgs = apply ^. aSpecialArgs & void
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression :: MonadNaming m => OldExpression m a -> m (NewExpression m a)
toExpression = rBody (toBody toExpression)

withFieldParam ::
    MonadNaming m => FuncParam (FieldParamInfo (OldName m) (TM m)) ->
    CPS m (FuncParam (FieldParamInfo (NewName m) (TM m)))
withFieldParam (FuncParam ann (FieldParamInfo (Tag info name tActions) fpActions)) =
    Tag info
    <$> opWithParamName GetFieldParameter varInfo name
    <*> liftCPS (toTagSelection tActions)
    <&> FieldParamInfo ?? fpActions
    <&> FuncParam ann
    where
        varInfo = ann ^. aInferredType & isFunctionType

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (TM m) ->
    CPS m (BinderParams (NewName m) (TM m))
withBinderParams BinderWithoutParams = pure BinderWithoutParams
withBinderParams (NullParam a) = pure (NullParam a)
withBinderParams (VarParam fp) =
    opWithParamName GetParameter (isFunctionType (fp ^. fpAnnotation . aInferredType))
    (fp ^. fpInfo . vpiName)
    <&> VarParam . \newName -> fp & fpInfo . vpiName .~ newName
withBinderParams (FieldParams xs) =
    traverse withFieldParam xs <&> FieldParams

toDefinitionBody ::
    MonadNaming m => (a -> m b) ->
    DefinitionBody (OldName m) (TM m) a ->
    m (DefinitionBody (NewName m) (TM m) b)
toDefinitionBody _ (DefinitionBodyBuiltin bi) = pure (DefinitionBodyBuiltin bi)
toDefinitionBody f (DefinitionBodyExpression (DefinitionExpression typeInfo presMode content)) =
     toBinder f content
     <&> DefinitionExpression typeInfo presMode
     <&> DefinitionBodyExpression

toDef ::
    MonadNaming m => (a -> m b) ->
    Definition (OldName m) (TM m) a ->
    m (Definition (NewName m) (TM m) b)
toDef f def@Definition {..} =
    do
        name <- opGetName Nothing DefName _drName
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
