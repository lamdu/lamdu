{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.Sugar.RedundantTypes
    ( redundantTypes
    ) where

import           Prelude.Compat

import           Control.Lens (Traversal')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Lamdu.Sugar.Types

redundantTypesDefaultTop :: Bool -> Traversal' (Expression name m a) (Payload m a)
redundantTypesDefaultTop topRedundant f (Expression body pl) =
    case body of
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetFieldParameter }) -> redundant
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetParameter }) -> redundant
    BodyLiteralInteger {} -> redundant
    BodyRecord{} -> redundant
    BodyList{} -> redundantChildren
    BodyToNom nom ->
        nom & Lens.traversed . redundantTypesDefaultTop True %%~ f
        <&> BodyToNom & mk
    BodyApply (Apply func specialArgs annotatedArgs) ->
        Apply
        <$> ( func & redundantTypesDefaultTop True %%~ f )
        <*> ( specialArgs & Lens.traversed recurse )
        <*> ( annotatedArgs & Lens.traversed . Lens.traversed %%~ recurse )
        <&> BodyApply & mk
    BodyCase (Case kind alts caseTail mAddAlt entityId) ->
        Case
        <$> (kind & Lens.traversed . redundantTypesDefaultTop True %%~ f)
        <*> ( alts
              & Lens.traversed . Lens.traversed
              . rBody . _BodyLam %%~ altLam)
        <*> (caseTail & Lens.traversed %%~ recurse)
        <*> pure mAddAlt
        <*> pure entityId
        <&> BodyCase & mk
        where
            altLam (Binder mPres mScope params lets bod mAct scopes) =
                Binder
                <$> pure mPres <*> pure mScope <*> pure params
                <*> (lets & Lens.traversed . Lens.traversed %%~ recurse)
                <*> (bod & redundantTypesDefaultTop True %%~ f)
                <*> pure mAct <*> pure scopes
    _ -> mk recBody
    where
        recurse = redundantTypes f
        mk newBody =
            Expression <$> newBody <*> (if topRedundant then f else pure) pl
        recBody = body & Lens.traversed recurse
        redundant = Expression <$> recBody <*> f pl
        redundantChildren =
            body & Lens.traversed . redundantTypesDefaultTop True %%~ f & mk

redundantTypes :: Traversal' (Expression name m a) (Payload m a)
redundantTypes = redundantTypesDefaultTop False
