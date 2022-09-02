{-# LANGUAGE TypeApplications, ScopedTypeVariables, GADTs, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, DefaultSignatures #-}
module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Hyper
import           Hyper.Recurse
import           Hyper.Infer.Blame (BlameResult(..), blame)
import qualified Hyper.Syntax.Row as Row
import           Hyper.Type.Functor (F)
import           Hyper.Type.Prune (Prune(..), _Unpruned)
import           Hyper.Unify (UVar)
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.New (newUnbound)
import qualified Lamdu.Calc.Infer as Infer
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Ops (newHole)
import           Lamdu.Expr.IRef (ValI, globalId, iref, writeValI, newValI)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Hyper (HRef, HStore)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude
import qualified Lamdu.Expr.IRef as ExprIRef

type T = Transaction

data PriorityClass
    = HealPoint
    | TypeAnnotation
    | InFragment
    | Other
    deriving (Eq, Ord)

type Priority = (PriorityClass, Int)

data NodeResult m t = UseNode (F (IRef m) t) | Fragment (F (IRef m) t)

data NodeInfo m t = NodeInfo
    { _priority :: Priority
    , _onUnify :: NodeResult m t
    , _onNoUnify :: NodeResult m t
    , _origPos :: F (IRef m) t
    }

Lens.makeLenses ''NodeInfo

class HTraversable h => Heal h where
    fixPriorities :: Ann (NodeInfo m) # h -> Ann (NodeInfo m) # h
    fixPriorities = id
    fragment :: Monad m => F (IRef m) # h -> T m (F (IRef m) # h)
    writeNode :: Monad m => F (IRef m) # h -> h # F (IRef m) -> T m ()

    healRecursive :: RecMethod Heal h
    default healRecursive :: DefRecMethod Heal h
    healRecursive _ = Dict

instance Heal Term where
    fixPriorities x@(Ann n b) =
        case b of
        V.BRecExtend r -> V.BRecExtend r & res (-1)
        V.BCase c -> c & Row.eVal . score +~ (-1) & V.BCase & res (-1)
        V.BApp a ->
            case a ^. V.appFunc . hVal of
            V.BLeaf V.LGetField{} -> a & V.appArg . score +~ (-1) & V.BApp & res 1
            _ -> a & V.appFunc . score +~ (-1) & V.BApp & res 0
        _ -> x
        where
            res diff = n & priority . _2 +~ diff & Ann
            score = hAnn . priority . _2
    fragment pos = newHole >>= newValI . V.BApp . (`V.App` pos)
    writeNode = writeValI

instance Heal (HCompose Prune T.Type) where
    fragment _ = newValI (_HCompose # Pruned)
    writeNode = writeValI

instance Heal (HCompose Prune T.Row) where
    fragment _ = newValI (_HCompose # Pruned)
    writeNode = writeValI

instance Recursive Heal where recurse = healRecursive . proxyArgument

prepareInFragExpr ::
    forall m h.
    (Monad m, Recursively Heal h) =>
    Ann (HRef m) # h ->
    Ann (NodeInfo m) # h
prepareInFragExpr (Ann a v) =
    withDict (recursively (Proxy @(Heal h))) $
    hmap (Proxy @(Recursively Heal) #> prepareInFragExpr) v
    & Ann (NodeInfo (InFragment, 0) (UseNode i) (Fragment i) i)
    & fixPriorities
    where
        i = a ^. iref

class (HStore m (HCompose Prune h), HFunctor h) => PrepareParamType m h
instance Monad m => PrepareParamType m T.Type
instance Monad m => PrepareParamType m T.Row

prepareParamType ::
    forall m h.
    (Monad m, Recursively (PrepareParamType m) h) =>
    Ann (HRef m) # HCompose Prune h ->
    Ann (NodeInfo m) # HCompose Prune h
prepareParamType (Ann a b) =
    withDict (recursively (Proxy @(PrepareParamType m h))) $
    b
    & hcomposed _Unpruned %~
        hmap (Proxy @(Recursively (PrepareParamType m)) #> _HCompose %~ prepareParamType)
    & Ann (NodeInfo (TypeAnnotation, 0) (UseNode i) (Fragment i) i)
    where
        i = a ^. iref

prepare ::
    Monad m =>
    ValI m ->
    Ann (HRef m) # Term ->
    Ann (NodeInfo m) # Term
prepare fragI (Ann a v) =
    if fragI == a ^. iref
    then
        fragmented ^. hVal & hmap (Proxy @(Recursively Heal) #> prepareInFragExpr)
        & Ann (NodeInfo (HealPoint, 0) (UseNode fi) (UseNode i) fi)
    else
        hmap
        ( \case
            HWitness V.W_Term_Term -> prepare fragI
            HWitness V.W_Term_HCompose_Prune_Type -> prepareParamType
        ) v
        & Ann (NodeInfo (Other, 0) (UseNode i) (Fragment i) i)
    & fixPriorities
    where
        fragmented = v ^?! V._BApp . V.appArg
        fi = fragmented ^. hAnn . iref
        i = a ^. iref

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
            \frag ->
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
                prepare frag topLevelExpr
                    & blame (^. priority) (_ANode # topLevelType)
                    & local (addRecursiveRef . addDeps)
            & Infer.runPureInfer V.emptyScope
                (Infer.InferState Infer.emptyPureInferState Infer.varGen)
            & either (error "bug in heal!") (write (topLevelExpr ^. hAnn) . (^. _1))
            >> postProcess

nodeResult :: (Heal t, Monad m) => (NodeInfo m :*: BlameResult UVar) # t -> T m (F (IRef m) # t)
nodeResult ann =
    case use of
    UseNode n -> pure n
    Fragment n -> fragment n
    where
        use =
            case ann of
            (info :*: Good{}) -> info ^. onUnify
            (info :*: _) -> info ^. onNoUnify

write :: Monad m => HRef m # Term -> Ann (NodeInfo m :*: BlameResult UVar) # Term -> T m ()
write top expr =
    do
        expr ^. hAnn & nodeResult >>= top ^. ExprIRef.setIref
        unwrapM (Proxy @Heal #>> modify) expr & void

modify ::
    forall t m.
    (Heal t, Monad m) =>
    Ann (NodeInfo m :*: BlameResult UVar) # t ->
    T m (t # Ann (NodeInfo m :*: BlameResult UVar))
modify (Ann ann body) =
    withDict (recurse (Proxy @(Heal t))) $
    do
        htraverse (Proxy @Heal #> nodeResult . (^. hAnn)) body >>= writeNode (ann ^. _1 . origPos)
        body & pure