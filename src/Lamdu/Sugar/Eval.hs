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
        Annotated (Payload EvalPrep n i o, a) # t0 ->
        Annotated (Payload (EvaluationScopes InternalName i) n i o, a) # t1

instance AddEvalToNode n i o (Const x) (Const x) where
    addToNode r (Ann (Const pl) (Const x)) = Ann (Const (pl & _1 %~ addToPayload r)) (Const x)

instance AddEval e => AddEvalToNode n i o (e EvalPrep n i o) (e (EvaluationScopes InternalName i) n i o) where
    addToNode results (Ann a b) =
        Ann
        { _hAnn = a & Lens._Wrapped . _1 %~ addToPayload results
        , _hVal = addToBody results i b
        }
        where
            i = a ^. Lens._Wrapped . _1 . plEntityId

type AddToBodyType e n i o a =
    AddEvalCtx -> EntityId -> Body e EvalPrep n i o a -> Body e (EvaluationScopes InternalName i) n i o a

class AddEval e where
    addToBody :: Applicative i => AddToBodyType e n i o a
    default addToBody ::
        ( HMorphWithConstraint (e EvalPrep n i o) (e (EvaluationScopes InternalName i) n i o) (AddEvalToNode n i o)
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

instance AddEval Case
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
instance AddEval InjectContent
instance AddEval LabeledApply
instance AddEval Let

instance AddEval Term where
    addToBody r i =
        \case
        BodyPlaceHolder -> BodyPlaceHolder
        BodyGetVar x -> BodyGetVar x
        BodyLiteral x -> BodyLiteral x
        BodyFromNom x -> BodyFromNom x
        BodySimpleApply (App x y) -> App (addToNode r x) (addToNode r y) & BodySimpleApply
        BodyGetField (GetField x t) -> GetField (addToNode r x) t & BodyGetField
        BodyRecord c -> addToBody r i c & BodyRecord
        BodyInject x -> x & iContent %~ addToBody r i & BodyInject
        BodyIfElse x -> addToBody r i x & BodyIfElse
        BodyLam lam -> lam & lamFunc %~ addToBody r i & BodyLam
        BodyToNom nom -> nom & nVal %~ addToNode r & BodyToNom
        BodyHole h -> h & holeOptions . Lens.mapped . Lens.mapped %~ addToHoleOption & BodyHole
        BodyCase x -> addToBody r i x & BodyCase
        BodyLabeledApply x -> addToBody r i x & BodyLabeledApply
        BodyFragment (Fragment f h t o) ->
            Fragment (addToNode r f) h t (o <&> Lens.mapped %~ addToHoleOption) & BodyFragment
        where
            addToHoleOption (HoleOption hi e s) =
                HoleOption hi (e <&> addToNode r)
                (s <&> _2 . Lens.mapped . holeResultConverted %~ addToNode r)

addToPayload ::
    Applicative i =>
    AddEvalCtx ->
    Payload EvalPrep name i o ->
    Payload (EvaluationScopes InternalName i) name i o
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
    WorkArea EvalPrep name i (T m) (Payload EvalPrep name i (T m), a) ->
    T m (
        WorkArea (EvaluationScopes InternalName i) name i (T m)
        (Payload (EvaluationScopes InternalName i) name i (T m), a))
addEvaluationResults cp r wa@(WorkArea panes repl listGlobals) =
    makeNominalsMap (wa ^.. SugarLens.workAreaEvalResults . eType . tIds) <&> AddEvalCtx r
    <&>
    \ctx ->
    WorkArea
    ( panes <&> SugarLens.paneBinder %~ addToNode ctx)
    ( repl
        & replExpr %~ addToNode ctx
        & replResult .~ (r <&> (^. erCompleted) & ConvertEval.completion cp)
        )
    listGlobals
