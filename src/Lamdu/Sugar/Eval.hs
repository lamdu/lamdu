{-# LANGUAGE TemplateHaskell, TypeApplications, RecordWildCards, ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DefaultSignatures #-}

module Lamdu.Sugar.Eval
    ( addEvaluationResults
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Data.Kind (Type)
import           Hyper
import           Hyper.Class.Morph
import           Hyper.Syntax.Nominal (NominalDecl)
import           Lamdu.Calc.Lens (tIds)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam, erCompleted, extractField)
import           Lamdu.Eval.Results.Process (addTypes)
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Load (makeNominalsMap)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types hiding (Type)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data AddEvalCtx = AddEvalCtx
    { _evalResults :: CurAndPrev EvalResults
    , _nominalsMap :: Map NominalId (Pure # NominalDecl T.Type)
    }

Lens.makeLenses ''AddEvalCtx

class AddEvalToNode i n t0 t1 where
    addToNode ::
        (Monad m, Applicative i) =>
        AddEvalCtx ->
        Annotated (ConvertPayload m (Annotation EvalPrep n, a)) # t0 ->
        Annotated (ConvertPayload m (Annotation (EvaluationScopes InternalName i) n, a)) # t1

instance AddEvalToNode i n (Const x) (Const x) where
    addToNode r (Ann (Const pl) (Const x)) = Ann (Const (addToPayload r pl)) (Const x)

instance
    (AddEval i n e, Applicative i) =>
    AddEvalToNode i n
        (e (Annotation EvalPrep n) n i o)
        (e (Annotation (EvaluationScopes InternalName i) n) n i o) where
    addToNode results (Ann a b) =
        Ann
        { _hAnn = a & Lens._Wrapped %~ addToPayload results
        , _hVal = addToBody results (a ^. Lens._Wrapped . pInput . Input.entityId) b
        }

type AddToBodyType e n (i :: Type -> Type) (o :: Type -> Type) m a =
    AddEvalCtx -> EntityId ->
    e (Annotation EvalPrep n) n i o #
        Annotated (ConvertPayload m (Annotation EvalPrep n, a)) ->
    e (Annotation (EvaluationScopes InternalName i) n) n i o #
        Annotated (ConvertPayload m (Annotation (EvaluationScopes InternalName i) n, a))

class AddEval i n e where
    addToBody :: (Applicative i, Monad m) => AddToBodyType e n i o m a
    default addToBody ::
        ( HMorphWithConstraint
            (e (Annotation EvalPrep n) n i o)
            (e (Annotation (EvaluationScopes InternalName i) n) n i o)
            (AddEvalToNode i n)
        , Applicative i, Monad m
        ) => AddToBodyType e n i o m a
    addToBody r _ =
        morphMap (Proxy @(AddEvalToNode i n) #?> addToNode r)

instance AddEval i n Assignment where
    addToBody r i (BodyFunction x) = addToBody r i x & BodyFunction
    addToBody r i (BodyPlain x) = x & apBody %~ addToBody r i & BodyPlain

instance AddEval i n Binder where
    addToBody r i = bBody %~ addToBody r i

instance AddEval i n BinderBody where
    addToBody r i (BinderLet x) = addToBody r i x & BinderLet
    addToBody r i (BinderTerm x) = addToBody r i x & BinderTerm

instance AddEval i n Composite

instance AddEval i n Else where
    addToBody r i (SimpleElse x) = addToBody r i x & SimpleElse
    addToBody r i (ElseIf x) = x & eIfElse %~ addToBody r i & ElseIf

instance AddEval i n Function where
    addToBody ctx i x@Function{..} =
        x
        { _fParams = addToParams ctx i _fParams
        , _fBody = addToNode ctx _fBody
        , _fBodyScopes =
            ctx ^. evalResults
            <&> (^. erAppliesOfLam . Lens.ix u)
            <&> Lens.mapped . Lens.mapped %~ BinderParamScopeId . (^. _1)
        }
        where
            EntityId u = i

instance AddEval i n IfElse
instance AddEval i n LabeledApply
instance AddEval i n Let
instance AddEval i n PostfixApply
instance AddEval i n PostfixFunc

instance AddEval i n Term where
    addToBody r i =
        \case
        BodyLeaf x -> BodyLeaf x
        BodySimpleApply (App x y) -> App (addToNode r x) (addToNode r y) & BodySimpleApply
        BodyRecord c -> addToBody r i c & BodyRecord
        BodyIfElse x -> addToBody r i x & BodyIfElse
        BodyLam lam -> lam & lamFunc %~ addToBody r i & BodyLam
        BodyToNom nom -> nom & nVal %~ addToNode r & BodyToNom
        BodyLabeledApply x -> addToBody r i x & BodyLabeledApply
        BodyFragment f -> f & fExpr %~ addToNode r & BodyFragment
        BodyPostfixApply x -> addToBody r i x & BodyPostfixApply
        BodyPostfixFunc x -> addToBody r i x & BodyPostfixFunc
        BodyNullaryInject (NullaryInject j e) ->
            NullaryInject (addToNode r j) (addToNode r e) & BodyNullaryInject

addToParams ::
    Applicative i =>
    AddEvalCtx -> EntityId ->
    Params (Annotation EvalPrep n) n i o ->
    Params (Annotation (EvaluationScopes InternalName i) n) n i o
addToParams ctx i =
    \case
    NullParam (p, a) ->
        NullParam
        ( p & fpAnnotation . _AnnotationVal %~
            ConvertEval.param (EntityId.ofEvalOf i) . appliesOfLam
        , a
        )
    VarParam (fp, info) ->
        VarParam
        ( fp & fpAnnotation . _AnnotationVal %~
            ConvertEval.param (EntityId.ofEvalOf (info ^. vpiTag . oTag . tagRefTag . tagInstance)) .
            appliesOfLam
        , info
        )
    RecordParams ps ->
        ps
        & SugarLens.taggedListItems %~ fixItem
        & RecordParams
    where
        EntityId u = i
        appliesOfLam v =
            ctx ^. evalResults
            <&> (^. erAppliesOfLam . Lens.at u)
            <&> fromMaybe mempty
            <&> Lens.mapped . Lens.mapped . _2 %~ addTypes (ctx ^. nominalsMap) (v ^. eType)
        fixItem taggedItem =
            taggedItem & tiValue . fpAnnotation . _AnnotationVal %~
            \v ->
            ctx ^. evalResults
            <&> (^. erAppliesOfLam . Lens.at u)
            <&> fromMaybe mempty
            <&> Lens.mapped . Lens.mapped . _2 %~
                addTypes (ctx ^. nominalsMap) (v ^. eType) .
                extractField () (t ^. tagVal)
            & ConvertEval.param (EntityId.ofEvalOf (t ^. tagInstance))
            where
                t = taggedItem ^. tiTag . tagRefTag

addToPayload ::
    Applicative i =>
    AddEvalCtx ->
    ConvertPayload m (Annotation EvalPrep n, a) ->
    ConvertPayload m (Annotation (EvaluationScopes InternalName i) n, a)
addToPayload ctx a =
    a
    & pInput . Input.userData . _1 . _AnnotationVal %~
        \v ->
        ctx ^. evalResults
        <&> (^. erExprValues . Lens.at u)
        <&> fromMaybe mempty
        <&> Lens.mapped %~ addTypes (ctx ^. nominalsMap) (v ^. eType)
        & ConvertEval.results (EntityId.ofEvalOf i)
    where
        EntityId u = i
        i = a ^. pInput . Input.entityId

addEvaluationResults ::
    forall n m i a.
    (Monad m, Applicative i) =>
    Anchors.CodeAnchors m ->
    CurAndPrev EvalResults ->
    WorkArea (Annotation EvalPrep n) n i (T m) (ConvertPayload m (Annotation EvalPrep n, a)) ->
    T m (
        WorkArea (Annotation (EvaluationScopes InternalName i) n) n i (T m)
        (ConvertPayload m (Annotation (EvaluationScopes InternalName i) n, a)))
addEvaluationResults cp r wa@(WorkArea panes repl globals) =
    makeNominalsMap
    ( wa ^..
        SugarLens.annotations @(Annotation EvalPrep n)
        . _AnnotationVal . eType . tIds
    )
    <&> AddEvalCtx r
    <&>
    \ctx ->
    WorkArea
    ( panes <&> SugarLens.paneBinder %~ addToNode ctx)
    ( repl
        & replExpr %~ addToNode ctx
        & replResult .~ (r <&> (^. erCompleted) & ConvertEval.completion cp)
        )
    globals
