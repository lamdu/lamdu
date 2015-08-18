{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.GUI.RedundantAnnotations
    ( markAnnotationsToDisplay
    ) where

import           Prelude.Compat

import           Control.Lens (Traversal')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Lamdu.GUI.ExpressionGui.Types as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

redundantAnnotationsDefaultTop :: Bool -> Traversal' (Expression name m a) (Payload m a)
redundantAnnotationsDefaultTop topRedundant f e@(Expression body pl) =
    case body of
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetFieldParameter }) -> redundant e
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetParameter }) -> redundant e
    BodyLiteralInteger {} -> redundant e
    BodyRecord{} -> redundant e
    BodyList{} -> redundantChildren
    BodyToNom nom ->
        nom & Lens.traversed %%~ redundantTop
        <&> BodyToNom & mk
    BodyApply (Apply func specialArgs annotatedArgs) ->
        Apply
        <$> redundantTop func
        <*> Lens.traversed recurse specialArgs
        <*> (annotatedArgs & Lens.traversed . Lens.traversed %%~ recurse)
        <&> BodyApply & mk
    BodyCase (Case kind alts caseTail mAddAlt entityId) ->
        Case
        <$> (kind & Lens.traversed %%~ redundantTop)
        <*> ( alts
              & Lens.traversed . Lens.traversed %%~
                SugarLens.bitraverseExpression (_BodyLam %%~ altLam) f)
        <*> (caseTail & Lens.traversed %%~ recurse)
        <*> pure mAddAlt
        <*> pure entityId
        <&> BodyCase & mk
    BodyLam _ -> redundant e
    _ -> Lens.traversed recurse body & mk
    where
        altLam = onBinder (Lens.traversed . Lens.traversed %%~ recurse) redundantTop
        onBinder onLets onBody (Binder mPres mScope params lets bod mAct scopes) =
            Binder mPres mScope params
            <$> onLets lets
            <*> onBody bod
            <*> pure mAct <*> pure scopes
        redundantTop = redundantAnnotationsDefaultTop True f
        recurse = redundantAnnotationsDefaultTop False f
        mk newBody =
            Expression <$> newBody <*> (if topRedundant then f else pure) pl
        redundant = SugarLens.bitraverseExpression (Lens.traversed recurse) f
        redundantChildren =
            body & Lens.traversed %%~ redundantTop & mk

redundantAnnotations :: Traversal' (Expression name m a) (Payload m a)
redundantAnnotations = redundantAnnotationsDefaultTop False

markAnnotationsToDisplay :: T.SugarExpr m -> T.SugarExpr m
markAnnotationsToDisplay v =
    v
    & SugarLens.subExprsOf _BodyToNom   . showAnn . T.showInEvalMode .~ T.EvalModeShowNothing
    & SugarLens.subExprsOf _BodyFromNom . showAnn . T.showInEvalMode .~ T.EvalModeShowNothing
    & SugarLens.payloadsOf _BodyInject  . showAnn . T.showInEvalMode .~ T.EvalModeShowNothing
    & redundantAnnotations                    . showAnn %~
      (T.showInTypeMode .~ False) .
      (T.showInEvalMode .~ T.EvalModeShowNothing) -- TODO: This makes little sense
    & SugarLens.holePayloads                  . showAnn %~
      (T.showTypeWhenMissing .~ True) .
      (T.showInEvalMode .~ T.EvalModeShowType)
    & SugarLens.holeArgs                      . showAnn %~
      (T.showTypeWhenMissing .~ True) .
      (T.showInEvalMode %~ don'tShowNothing)
    where
        don'tShowNothing T.EvalModeShowNothing = T.EvalModeShowType
        don'tShowNothing x = x
        showAnn = plData . T.plShowAnnotation
