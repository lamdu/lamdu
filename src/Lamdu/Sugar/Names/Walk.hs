{-# LANGUAGE FlexibleContexts, TypeFamilies, RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), _GlobalDef, _TaggedVar, _TaggedNominal, _Tag
    , isLocal, isGlobal
    , FunctionSignature(..), Disambiguator
    , NameConvertor, CPSNameConvertor
    , OldExpression, NewExpression
    , toWorkArea, toDef, toExpression, toBody
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

-- TODO: Maybe remove "TaggedNominal", make it a disambiguation context?
data NameType = GlobalDef | TaggedVar | TaggedNominal | Tag
    deriving (Eq, Ord, Show)

Lens.makePrisms ''NameType

-- | Bound by a local binder. Used to determine which locals collide
isLocal :: NameType -> Bool
isLocal TaggedVar = True
isLocal _ = False

-- | Not bound by a local binder and not a tag
--
-- A context-less tag is not considered a global, because it is not an
-- actual entity. Used to determine collisions of binder-less names
isGlobal :: NameType -> Bool
isGlobal GlobalDef = True
isGlobal TaggedNominal = True
isGlobal _ = False

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

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

    opWithName :: NameGen.VarInfo -> NameType -> CPSNameConvertor m
    opGetName :: Maybe Disambiguator -> NameType -> NameConvertor m

type TM m = T (SM m)

type OldExpression m a = Expression (OldName m) (TM m) a
type NewExpression m a = Expression (NewName m) (TM m) a

isFunctionType :: Type -> NameGen.VarInfo
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NormalVar

toParamRef ::
    MonadNaming m =>
    ParamRef (OldName m) p ->
    m (ParamRef (NewName m) p)
toParamRef = (pNameRef . nrName) (opGetName Nothing TaggedVar)

binderVarType :: BinderVarForm t -> NameType
binderVarType GetLet = TaggedVar
binderVarType (GetDefinition _) = GlobalDef

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
    traverse (opGetName Nothing Tag) x <&> GetParamsRecord

toNodeActions ::
    MonadNaming m =>
    NodeActions (OldName m) (TM m) ->
    m (NodeActions (NewName m) (TM m))
toNodeActions = wrapInRecord toTagSelection

toLet ::
    MonadNaming m => (a -> m b) ->
    Let (OldName m) (TM m) a ->
    m (Let (NewName m) (TM m) b)
toLet expr Let{..} =
    do
        (_lName, _lBody) <-
            unCPS (withTag TaggedVar (isFunctionType (_lAnnotation ^. aInferredType)) _lName)
            (toBinderBody expr _lBody)
        _lValue <- toBinder expr _lValue
        _lActions <- laNodeActions toNodeActions _lActions
        pure Let{..}

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

toBinderActions ::
    MonadNaming m =>
    BinderActions (OldName m) (TM m) ->
    m (BinderActions (NewName m) (TM m))
toBinderActions BinderActions{..} =
    BinderActions
    <$> _PrependParam toTagSelection _baAddFirstParam
    <*> Lens._Just toNodeActions _baMNodeActions

toBinder ::
    MonadNaming m => (a -> m b) ->
    Binder (OldName m) (TM m) a ->
    m (Binder (NewName m) (TM m) b)
toBinder expr Binder{..} =
    (\(_bParams, _bBody) _bActions -> Binder{..})
    <$> unCPS (withBinderParams _bParams) (toBinderBody expr _bBody)
    <*> toBinderActions _bActions

toLam ::
    MonadNaming m => (a -> m b) ->
    Lambda (OldName m) (TM m) a ->
    m (Lambda (NewName m) (TM m) b)
toLam = lamBinder . toBinder

toTagSelection ::
    MonadNaming m =>
    TagSelection (OldName m) (TM m) a -> m (TagSelection (NewName m) (TM m) a)
toTagSelection t@TagSelection{..} =
    do
        run0 <- opRun
        run1 <- opRun
        pure t
            { _tsOptions =
                _tsOptions >>= run0 . (traverse . toName) (opGetName Nothing Tag)
            , _tsNewTag = _tsNewTag >>= run1 . _1 (opGetName Nothing Tag)
            }

toTagOf ::
    MonadNaming m =>
    NameType -> Sugar.Tag (OldName m) (TM m) ->
    m (Sugar.Tag (NewName m) (TM m))
toTagOf nameType (Sugar.Tag info name actions) =
    Sugar.Tag info
    <$> opGetName Nothing nameType name
    <*> toTagSelection actions

withTag ::
    MonadNaming m =>
    NameType -> NameGen.VarInfo ->
    Sugar.Tag (OldName m) (TM m) ->
    CPS m (Sugar.Tag (NewName m) (TM m))
withTag nameType varInfo (Sugar.Tag info name actions) =
    Sugar.Tag info
    <$> opWithName varInfo nameType name
    <*> liftCPS (toTagSelection actions)

toRelayedArg ::
    MonadNaming m =>
    RelayedArg (OldName m) (TM m) ->
    m (RelayedArg (NewName m) (TM m))
toRelayedArg RelayedArg{..} =
    (\_raValue _raActions -> RelayedArg{..})
    <$> toParamRef _raValue
    <*> toNodeActions _raActions

toLabeledApply ::
    MonadNaming m =>
    (a -> m b) ->
    LabeledApply (OldName m) (TM m) a ->
    m (LabeledApply (NewName m) (TM m) b)
toLabeledApply expr app@LabeledApply{..} =
    LabeledApply
    <$>
    ( _aFunc & bvNameRef . nrName %%~
        opGetName (Just (funcSignature app)) TaggedVar
    )
    <*> pure _aSpecialArgs
    <*> (traverse . aaName) (opGetName Nothing Tag) _aAnnotatedArgs
    <*> traverse toRelayedArg _aRelayedArgs
    >>= traverse expr

toHole ::
    MonadNaming m =>
    Hole (TM m) (Expression (OldName m) (TM m) ()) ->
    m (Hole (TM m) (Expression (NewName m) (TM m) ()))
toHole hole =
    opRun
    <&>
    \run ->
    SugarLens.holeTransformExprs (run . toExpression) hole

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
            , _fOptions = _fOptions <&> Lens.mapped %~ SugarLens.holeOptionTransformExprs (run . toExpression)
            }

toComposite ::
    MonadNaming m =>
    (a -> m b) ->
    Composite (OldName m) (TM m) a ->
    m (Composite (NewName m) (TM m) b)
toComposite expr Composite{..} =
    (\_cItems _cAddItem -> Composite{..})
    <$> (traverse . ciTag) (toTagOf Tag) _cItems
    <*> toTagSelection _cAddItem
    >>= traverse expr

toCase ::
    MonadNaming m =>
    (a -> m b) ->
    Case (OldName m) (TM m) a ->
    m (Case (NewName m) (TM m) b)
toCase expr (Case k c) = Case <$> traverse expr k <*> toComposite expr c

toElseIfContent ::
    MonadNaming m =>
    (a -> m b) ->
    ElseIfContent (OldName m) (TM m) a ->
    m (ElseIfContent (NewName m) (TM m) b)
toElseIfContent expr ElseIfContent{..} =
    (\_eiContent _eiNodeActions -> ElseIfContent{..})
    <$> toIfElse expr _eiContent
    <*> toNodeActions _eiNodeActions

toElse ::
    MonadNaming m =>
    (a -> m b) ->
    Else (OldName m) (TM m) a ->
    m (Else (NewName m) (TM m) b)
toElse expr (SimpleElse x) = expr x <&> SimpleElse
toElse expr (ElseIf x) = toElseIfContent expr x <&> ElseIf

toIfElse ::
    MonadNaming m =>
    (a -> m b) ->
    IfElse (OldName m) (TM m) a ->
    m (IfElse (NewName m) (TM m) b)
toIfElse expr (IfElse ifThen els_) =
    IfElse
    <$> traverse expr ifThen
    <*> toElse expr els_

toBody ::
    MonadNaming m => (a -> m b) ->
    Body (OldName m) (TM m) a ->
    m (Body (NewName m) (TM m) b)
toBody expr = \case
    BodyGetField     x -> x & traverse expr >>= gfTag toTag <&> BodyGetField
    BodyInject       x -> x & traverse expr >>= iTag toTag <&> BodyInject
    BodyRecord       x -> x & toComposite expr <&> BodyRecord
    BodyCase         x -> x & toCase expr <&> BodyCase
    BodyIfElse       x -> x & toIfElse expr <&> BodyIfElse
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
        toTag = toTagOf Tag
        toTId = tidName %%~ opGetName Nothing TaggedNominal

funcSignature :: LabeledApply name binderVar a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sSpecialArgs = apply ^. aSpecialArgs & void
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression :: MonadNaming m => OldExpression m a -> m (NewExpression m a)
toExpression (Expression body pl) =
    Expression
    <$> toBody toExpression body
    <*> plActions toNodeActions pl

withParam ::
    MonadNaming m => FuncParam (ParamInfo (OldName m) (TM m)) ->
    CPS m (FuncParam (ParamInfo (NewName m) (TM m)))
withParam (FuncParam ann (ParamInfo tag fpActions)) =
    ParamInfo
    <$> withTag TaggedVar varInfo tag
    <*> liftCPS ((fpAddNext . Sugar._AddNext) toTagSelection fpActions)
    <&> FuncParam ann
    where
        varInfo = ann ^. aInferredType & isFunctionType

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (TM m) ->
    CPS m (BinderParams (NewName m) (TM m))
withBinderParams BinderWithoutParams = pure BinderWithoutParams
withBinderParams (NullParam a) = pure (NullParam a)
withBinderParams (Params xs) = traverse withParam xs <&> Params

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
toDef f Definition{..} =
    do
        -- NOTE: A global def binding is not considered a binder, as
        -- it exists everywhere, not just inside the binding
        _drName <- toTagOf GlobalDef _drName
        _drBody <- toDefinitionBody f _drBody
        pure Definition{..}

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
