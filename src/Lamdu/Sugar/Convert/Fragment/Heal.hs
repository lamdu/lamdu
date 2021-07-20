{-# LANGUAGE TypeApplications, ScopedTypeVariables, GADTs, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Hyper
import           Hyper.Infer.Blame (BlameResult(..), blame)
import qualified Hyper.Syntax.Row as Row
import           Hyper.Type.Prune (Prune(..), _Unpruned)
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.New (newUnbound)
import qualified Lamdu.Calc.Infer as Infer
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, globalId, iref, writeValI)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Revision.Deltum.Hyper (HRef, HStore)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data EditAction a
    = OnUnify a
    | OnNoUnify a

data PriorityClass
    = HealPoint
    | TypeAnnotation
    | InFragment
    | Other
    deriving (Eq, Ord)

type Priority = (PriorityClass, Int)

class HFunctor h => Prepare h where
    fixPriorities ::
        Annotated ((a, Int), b) # h ->
        Annotated ((a, Int), b) # h
    fixPriorities = id
    wrap :: Monad m => HRef m # h -> T m ()

instance Prepare Term where
    fixPriorities x@(Ann (Const ((cat, priority), pl)) b) =
        case b of
        V.BRecExtend r -> V.BRecExtend r & res (-1)
        V.BCase c -> c & Row.eVal . score +~ (-1) & V.BCase & res (-1)
        V.BApp a ->
            case a ^. V.appFunc . hVal of
            V.BLeaf V.LGetField{} -> a & V.appArg . score +~ (-1) & V.BApp & res 1
            _ -> a & V.appFunc . score +~ (-1) & V.BApp & res 0
        _ -> x
        where
            res diff = Ann (Const ((cat, priority + diff), pl))
            score = annotation . _1 . _2
    wrap = void . DataOps.applyHoleTo

instance Prepare (HCompose Prune T.Type) where
    wrap a = writeValI (a ^. iref) (_HCompose # Pruned)

instance Prepare (HCompose Prune T.Row) where
    wrap a = writeValI (a ^. iref) (_HCompose # Pruned)

prepareInFragExpr ::
    forall m h.
    (Monad m, Recursively Prepare h) =>
    Ann (HRef m) # h ->
    Annotated (Priority, EditAction (T m ())) # h
prepareInFragExpr (Ann a v) =
    withDict (recursively (Proxy @(Prepare h))) $
    hmap (Proxy @(Recursively Prepare) #> prepareInFragExpr) v
    & Ann (Const ((InFragment, 0), OnNoUnify (wrap a)))
    & fixPriorities

class (HStore m (HCompose Prune h), HFunctor h) => PrepareParamType m h
instance Monad m => PrepareParamType m T.Type
instance Monad m => PrepareParamType m T.Row

prepareParamType ::
    forall m h.
    (Monad m, Recursively (PrepareParamType m) h) =>
    Ann (HRef m) # HCompose Prune h ->
    Annotated (Priority, EditAction (T m ())) # HCompose Prune h
prepareParamType (Ann a b) =
    withDict (recursively (Proxy @(PrepareParamType m h))) $
    Ann
    ( Const ((TypeAnnotation, 0)
    , OnNoUnify (writeValI (a ^. iref) (_HCompose # Pruned)))
    ) (b & hcomposed _Unpruned %~
        hmap (Proxy @(Recursively (PrepareParamType m)) #> _HCompose %~ prepareParamType))

prepare ::
    Monad m =>
    ValI m ->
    Ann (HRef m) # Term ->
    Annotated (Priority, EditAction (T m ())) # Term
prepare fragI (Ann a v) =
    if fragI == a ^. iref
    then
        fragmented ^. hVal & hmap (Proxy @(Recursively Prepare) #> prepareInFragExpr)
        & Ann (Const ((HealPoint, 0), OnUnify (() <$ DataOps.replace a (fragmented ^. hAnn . iref))))
    else
        hmap
        ( \case
            HWitness V.W_Term_Term -> prepare fragI
            HWitness V.W_Term_HCompose_Prune_Type -> prepareParamType
        ) v
        & Ann (Const ((Other, 0), OnNoUnify (() <$ DataOps.applyHoleTo a)))
    & fixPriorities
    where
        fragmented = v ^?! V._BApp . V.appArg

healMismatch :: Monad m => ConvertM m (ValI m -> T m ())
healMismatch =
    do
        postProcess <- ConvertM.postProcessAssert
        topLevelExpr <-
            Lens.view ConvertM.scTopLevelExpr
            <&> hflipped %~ hmap (const (^. Input.stored))
        deps <- Lens.view (ConvertM.scFrozenDeps . Property.pVal)
        recursiveRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
        pure $
            \fragment ->
            do
                topLevelType <- newUnbound
                let addRecursiveRef =
                        case recursiveRef of
                        Nothing -> id
                        Just rr ->
                            V.scopeVarTypes .
                            Lens.at (globalId (rr ^. ConvertM.rrDefI)) ?~
                            MkHFlip (GMono topLevelType)
                addDeps <- Infer.loadDeps deps
                prepare fragment topLevelExpr
                    & blame (^. Lens._Wrapped . _1) (_ANode # topLevelType)
                    & local (addRecursiveRef . addDeps)
            & Infer.runPureInfer V.emptyScope
                (Infer.InferState Infer.emptyPureInferState Infer.varGen)
            & either (error "bug in heal!")
                -- Doing the inner changes first and then the outer ones so that
                -- setting inner properties won't override outer iref values
                (sequence_ . reverse . hfoldMap (const act) . (^. _1 . hflipped))
            >> postProcess
    where
        act (Const (_, OnUnify x) :*: Good{}) = [x]
        act (Const (_, OnNoUnify x) :*: Mismatch{}) = [x]
        act _ = []
