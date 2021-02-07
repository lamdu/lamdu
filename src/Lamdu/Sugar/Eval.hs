{-# LANGUAGE TemplateHaskell, TypeApplications, RecordWildCards #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, DefaultSignatures #-}

module Lamdu.Sugar.Eval
    ( addEvaluationResults
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Hyper
import           Hyper.Class.Morph
import           Hyper.Type.AST.Nominal (NominalDecl)
import           Lamdu.Calc.Lens (tIds)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam, erCompleted, extractField)
import           Lamdu.Eval.Results.Process (addTypes)
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import           Lamdu.Sugar.Convert.Load (makeNominalsMap)
import           Lamdu.Sugar.Internal (InternalName, EvalPrep, eType)
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data AddEvalCtx = AddEvalCtx
    { _evalResults :: CurAndPrev EvalResults
    , _nominalsMap :: Map NominalId (Pure # NominalDecl T.Type)
    }

Lens.makeLenses ''AddEvalCtx

class AddEvalToNode n i o t0 t1 where
    addToNode ::
        Applicative i =>
        AddEvalCtx ->
        Annotated (Payload (Annotation EvalPrep n) n i o, a) # t0 ->
        Annotated (Payload (Annotation (EvaluationScopes InternalName i) n) n i o, a) # t1

instance AddEvalToNode n i o (Const x) (Const x) where
    addToNode r (Ann (Const pl) (Const x)) = Ann (Const (pl & _1 %~ addToPayload r)) (Const x)

instance AddEval e => AddEvalToNode n i o (e (Annotation EvalPrep n) n i o) (e (Annotation (EvaluationScopes InternalName i) n) n i o) where
    addToNode results (Ann a b) =
        Ann
        { _hAnn = a & Lens._Wrapped . _1 %~ addToPayload results
        , _hVal = addToBody results i b
        }
        where
            i = a ^. Lens._Wrapped . _1 . plEntityId

type AddToBodyType e n i o a =
    AddEvalCtx -> EntityId -> Body e (Annotation EvalPrep n) n i o a -> Body e (Annotation (EvaluationScopes InternalName i) n) n i o a

class AddEval e where
    addToBody :: Applicative i => AddToBodyType e n i o a
    default addToBody ::
        ( HMorphWithConstraint (e (Annotation EvalPrep n) n i o) (e (Annotation (EvaluationScopes InternalName i) n) n i o) (AddEvalToNode n i o)
        , Applicative i
        ) => AddToBodyType e n i o a
    addToBody r _ x =
        morphMap (p x #?> addToNode r) x
        where
            p :: Body t v n i o a -> Proxy (AddEvalToNode n i o)
            p _ = Proxy

instance AddEval Assignment where
    addToBody r i (BodyFunction x) = addToBody r i x & BodyFunction
    addToBody r i (BodyPlain x) = x & apBody %~ addToBody r i & BodyPlain

instance AddEval Binder where
    addToBody r i (BinderLet x) = addToBody r i x & BinderLet
    addToBody r i (BinderTerm x) = addToBody r i x & BinderTerm

instance AddEval Composite

instance AddEval Else where
    addToBody r i (SimpleElse x) = addToBody r i x & SimpleElse
    addToBody r i (ElseIf x) = addToBody r i x & ElseIf

instance AddEval Function where
    addToBody ctx i x@Function{..} =
        x
        { _fParams =
            case _fParams of
            NullParam (p, a) ->
                NullParam
                ( p & fpAnnotation . _AnnotationVal %~
                    ConvertEval.param (EntityId.ofEvalOf i) . appliesOfLam
                , a
                )
            Params [(fp, info)] ->
                Params
                [( fp & fpAnnotation . _AnnotationVal %~
                    ConvertEval.param (EntityId.ofEvalOf (info ^. piTag . tagRefTag . tagInstance)) .
                    appliesOfLam
                , info
                )]
            Params ps ->
                ps
                <&> fixParam
                & Params
                where
                    fixParam (fp, info) =
                        ( fp & fpAnnotation . _AnnotationVal %~
                            \v ->
                            ctx ^. evalResults
                            <&> (^. erAppliesOfLam . Lens.at u)
                            <&> fromMaybe mempty
                            <&> Lens.mapped . Lens.mapped . _2 %~
                                addTypes (ctx ^. nominalsMap) (v ^. eType) .
                                extractField () (info ^. piTag . tagRefTag . tagVal)
                            & ConvertEval.param (EntityId.ofEvalOf (info ^. piTag . tagRefTag . tagInstance))
                        , info
                        )
        , _fBody = addToNode ctx _fBody
        , _fBodyScopes =
            ctx ^. evalResults
            <&> (^. erAppliesOfLam . Lens.ix u)
            <&> Lens.mapped . Lens.mapped %~ BinderParamScopeId . (^. _1)
        }
        where
            appliesOfLam v =
                ctx ^. evalResults
                <&> (^. erAppliesOfLam . Lens.at u)
                <&> fromMaybe mempty
                <&> Lens.mapped . Lens.mapped . _2 %~ addTypes (ctx ^. nominalsMap) (v ^. eType)
            EntityId u = i

instance AddEval IfElse
instance AddEval LabeledApply
instance AddEval Let
instance AddEval PostfixApply
instance AddEval PostfixFunc

instance AddEval Term where
    addToBody r i =
        \case
        BodyPlaceHolder -> BodyPlaceHolder
        BodyGetVar x -> BodyGetVar x
        BodyLiteral x -> BodyLiteral x
        BodySimpleApply (App x y) -> App (addToNode r x) (addToNode r y) & BodySimpleApply
        BodyRecord c -> addToBody r i c & BodyRecord
        BodyInject x -> BodyInject x
        BodyEmptyInject x -> BodyEmptyInject x
        BodyIfElse x -> addToBody r i x & BodyIfElse
        BodyLam lam -> lam & lamFunc %~ addToBody r i & BodyLam
        BodyToNom nom -> nom & nVal %~ addToNode r & BodyToNom
        BodyHole h -> BodyHole h
        BodyLabeledApply x -> addToBody r i x & BodyLabeledApply
        BodyFragment f -> f & fExpr %~ addToNode r & BodyFragment
        BodyPostfixApply x -> addToBody r i x & BodyPostfixApply
        BodyPostfixFunc x -> addToBody r i x & BodyPostfixFunc

addToPayload ::
    Applicative i =>
    AddEvalCtx ->
    Payload (Annotation EvalPrep n0) name i o ->
    Payload (Annotation (EvaluationScopes InternalName i) n0) name i o
addToPayload ctx a =
    a
    & plAnnotation . _AnnotationVal %~
        \v ->
        ctx ^. evalResults
        <&> (^. erExprValues . Lens.at u)
        <&> fromMaybe mempty
        <&> Lens.mapped %~ addTypes (ctx ^. nominalsMap) (v ^. eType)
        & ConvertEval.results (EntityId.ofEvalOf i)
    where
        EntityId u = i
        i = a ^. plEntityId

addEvaluationResults ::
    (Applicative i, Monad m) =>
    Anchors.CodeAnchors m ->
    CurAndPrev EvalResults ->
    WorkArea (Annotation EvalPrep n) n i (T m) (Payload (Annotation EvalPrep n) n i (T m), a) ->
    T m (
        WorkArea (Annotation (EvaluationScopes InternalName i) n) n i (T m)
        (Payload (Annotation (EvaluationScopes InternalName i) n) n i (T m), a))
addEvaluationResults cp r wa@(WorkArea panes repl listGlobals) =
    makeNominalsMap
    (wa ^.. SugarLens.workAreaAnnotations . _AnnotationVal . eType . tIds)
    <&> AddEvalCtx r
    <&>
    \ctx ->
    WorkArea
    ( panes <&> SugarLens.paneBinder %~ addToNode ctx)
    ( repl
        & replExpr %~ addToNode ctx
        & replResult .~ (r <&> (^. erCompleted) & ConvertEval.completion cp)
        )
    listGlobals
