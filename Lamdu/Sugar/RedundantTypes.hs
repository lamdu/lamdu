{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.Sugar.RedundantTypes
    ( redundantTypes
    ) where

import           Prelude.Compat

import           Control.Lens (Traversal')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Lamdu.Sugar.Types
import qualified Lamdu.Sugar.Lens as SugarLens

redundantTypesDefaultTop :: Bool -> Traversal' (Expression name m a) (Payload m a)
redundantTypesDefaultTop topRedundant f e@(Expression body pl) =
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
        redundantTop = redundantTypesDefaultTop True f
        recurse = redundantTypesDefaultTop False f
        mk newBody =
            Expression <$> newBody <*> (if topRedundant then f else pure) pl
        redundant = SugarLens.bitraverseExpression (Lens.traversed recurse) f
        redundantChildren =
            body & Lens.traversed %%~ redundantTop & mk

redundantTypes :: Traversal' (Expression name m a) (Payload m a)
redundantTypes = redundantTypesDefaultTop False
