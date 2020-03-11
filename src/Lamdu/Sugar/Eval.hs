{-# LANGUAGE TemplateHaskell, TypeApplications, RecordWildCards #-}

module Lamdu.Sugar.Eval
    ( addEvaluationResults
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Hyper
import           Hyper.Type.AST.Nominal (NominalDecl)
import           Lamdu.Calc.Lens (tIds)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam, extractField)
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

class AddEval e where
    addToBody ::
        Applicative i =>
        AddEvalCtx ->
        EntityId ->
        Body e EvalPrep name i o a ->
        Body e (EvaluationScopes InternalName i) name i o a

instance AddEval Assignment where
    addToBody r i (BodyFunction x) = addToBody r i x & BodyFunction
    addToBody r i (BodyPlain x) = x & apBody %~ addToBody r i & BodyPlain

instance AddEval Binder where
    addToBody r i (BinderLet x) = addToBody r i x & BinderLet
    addToBody r i (BinderTerm x) = addToBody r i x & BinderTerm

instance AddEval Composite where
    addToBody r _ (Composite i p t a) =
        Composite (i <&> ciExpr %~ addToNode r) (p <&> addToConst r)
        (t & _OpenComposite . _2 %~ addToNode r) a

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
                            <&> Lens.mapped . Lens.mapped . Lens._2 %~
                                addTypes (ctx ^. nominalsMap) (v ^. eType) .
                                extractField () (info ^. piTag . tagRefTag . tagVal)
                            & ConvertEval.param (EntityId.ofEvalOf (info ^. piTag . tagRefTag . tagInstance))
                        , info
                        )
        , _fBody = addToNode ctx _fBody
        , _fBodyScopes =
            ctx ^. evalResults
            <&> (^. erAppliesOfLam . Lens.ix u)
            <&> Lens.mapped . Lens.mapped %~ BinderParamScopeId . (^. Lens._1)
        }
        where
            appliesOfLam v =
                ctx ^. evalResults
                <&> (^. erAppliesOfLam . Lens.at u)
                <&> fromMaybe mempty
                <&> Lens.mapped . Lens.mapped . Lens._2 %~ addTypes (ctx ^. nominalsMap) (v ^. eType)
            EntityId u = i


instance AddEval IfElse where
    addToBody r _ (IfElse i t e) = IfElse (addToNode r i) (addToNode r t) (addToNode r e)

instance AddEval InjectContent where
    addToBody r _ (InjectNullary x) = addToConst r x & InjectNullary
    addToBody r _ (InjectVal x) = addToNode r x & InjectVal

instance AddEval Let where
    addToBody r _ x@Let{..} =
        x
        { _lValue = addToNode r _lValue
        , _lBody = addToNode r _lBody
        }

instance AddEval Term where
    addToBody r i =
        \case
        BodyPlaceHolder -> BodyPlaceHolder
        BodyGetVar x -> BodyGetVar x
        BodyLiteral x -> BodyLiteral x
        BodyFromNom x -> BodyFromNom x
        BodySimpleApply (App x y) -> App (n x) (n y) & BodySimpleApply
        BodyGetField (GetField x t) -> GetField (n x) t & BodyGetField
        BodyRecord c -> addToBody r i c & BodyRecord
        BodyInject (Inject t c) -> addToBody r i c & Inject t & BodyInject
        BodyIfElse x -> addToBody r i x & BodyIfElse
        BodyLam lam -> lam & lamFunc %~ addToBody r i & BodyLam
        BodyToNom nom -> nom & nVal %~ addToNode r & BodyToNom
        BodyHole h -> h & holeOptions . Lens.mapped . Lens.mapped %~ addToHoleOption & BodyHole
        BodyCase (Case k b) ->
            Case (k & _CaseWithArg . caVal %~ addToNode r) (addToBody r i b) & BodyCase
        BodyLabeledApply (LabeledApply f s a p) ->
            LabeledApply (addToConst r f) (s <&> n) (a <&> aaExpr %~ n) (p <&> addToConst r) & BodyLabeledApply
        BodyFragment (Fragment f h t o) ->
            Fragment (addToNode r f) h t (o <&> Lens.mapped %~ addToHoleOption) & BodyFragment
        where
            n ::
                (AddEval e, Applicative i) =>
                Expr e EvalPrep name i o a ->
                Expr e (EvaluationScopes InternalName i) name i o a
            n = addToNode r
            addToHoleOption (HoleOption hi e s) =
                HoleOption hi (e <&> addToNode r)
                (s <&> Lens._2 . Lens.mapped . holeResultConverted %~ addToNode r)

addToNode ::
    (AddEval e, Applicative i) =>
    AddEvalCtx ->
    Expr e EvalPrep name i o a ->
    Expr e (EvaluationScopes InternalName i) name i o a
addToNode results (Ann a b) =
    Ann
    { _hAnn = a & Lens._Wrapped %~ addToPayload results
    , _hVal = addToBody results i b
    }
    where
        i = a ^. Lens._Wrapped . plEntityId

addToPayload ::
    Applicative i =>
    AddEvalCtx ->
    Payload EvalPrep name i o a ->
    Payload (EvaluationScopes InternalName i) name i o a
addToPayload ctx a =
    a
    & plAnnotation . _AnnotationVal %~
        \v ->
        ( ctx ^. evalResults
            <&> (^. erExprValues . Lens.at u)
            <&> fromMaybe mempty
            <&> Lens.mapped %~ addTypes (ctx ^. nominalsMap) (v ^. eType)
            & ConvertEval.results (EntityId.ofEvalOf i)
        )
    where
        EntityId u = i
        i = a ^. plEntityId

addToConst ::
    Applicative i =>
    AddEvalCtx ->
    Annotated (Payload EvalPrep name i o a) # Const x ->
    Annotated (Payload (EvaluationScopes InternalName i) name i o a) # Const x
addToConst r (Ann (Const pl) (Const x)) = Ann (Const (addToPayload r pl)) (Const x)

addEvaluationResults ::
    (Applicative i, Monad m) =>
    CurAndPrev EvalResults ->
    WorkArea EvalPrep name i o (Payload EvalPrep name i o a) ->
    T m (
        WorkArea (EvaluationScopes InternalName i) name i o
        (Payload (EvaluationScopes InternalName i) name i o a))
addEvaluationResults r (WorkArea panes repl listGlobals) =
    makeNominalsMap (evalPreps ^.. traverse . eType . tIds) <&> AddEvalCtx r
    <&>
    \ctx ->
    WorkArea
    ( panes <&>
        paneBody . _PaneDefinition . drBody .
        _DefinitionBodyExpression . deContent %~ addToNode ctx)
    (repl & replExpr %~ addToNode ctx)
    listGlobals
    where
        evalPreps =
            repl ^.. replExpr . SugarLens.evalResults <>
            panes ^..
                traverse . paneBody . _PaneDefinition . drBody .
                _DefinitionBodyExpression . deContent . SugarLens.evalResults
