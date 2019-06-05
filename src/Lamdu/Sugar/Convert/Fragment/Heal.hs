{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import           AST (Tree, monoChildren)
import           AST.Infer (Infer(..), ITerm(..), IResult(..))
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.Apply (applyArg)
import qualified AST.Term.Row as Row
import           AST.Unify (unify, applyBindings, newUnbound)
import           AST.Unify.Binding (UVar)
import           AST.Unify.Generalize (GTerm(..))
import qualified Control.Lens.Extended as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (MonadState(..), State, evalState)
import           Data.List (sortOn)
import qualified Data.Property as Property
import           Lamdu.Calc.Infer (PureInfer, runPureInfer, InferState(..), loadDeps, emptyPureInferState, varGen)
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, ValP, globalId)
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Input as Input
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data EditAction a
    = OnUnify a
    | OnNoUnify a
Lens.makePrisms ''EditAction

data PriorityClass
    = HealPoint
    | InFragment
    | Other
    deriving (Eq, Ord)

type Priority = (PriorityClass, Int)

fixPriorities ::
    Tree (Ann ((a, Int), b)) Term ->
    Tree (Ann ((a, Int), b)) Term
fixPriorities x@(Ann ((cat, priority), pl) b) =
    case b of
    V.BGetField g ->
        g & V.getFieldRecord . score +~ (-1) & V.BGetField
        & res 1
    V.BRecExtend r -> V.BRecExtend r & res (-1)
    V.BCase c -> c & Row.eVal . score +~ (-1) & V.BCase & res (-1)
    _ -> x
    where
        res diff = Ann ((cat, priority + diff), pl)
        score = ann . _1 . _2

prepareInFragExpr ::
    Monad m =>
    Tree (Ann (ValP m)) Term ->
    Tree (Ann (Priority, EditAction (T m ()))) Term
prepareInFragExpr (Ann a v) =
    v & monoChildren %~ prepareInFragExpr
    & Ann ((InFragment, 0), OnNoUnify (() <$ DataOps.applyHoleTo a))
    & fixPriorities

prepare ::
    Monad m =>
    ValI m ->
    Tree (Ann (ValP m)) Term ->
    Tree (Ann (Priority, EditAction (T m ()))) Term
prepare fragI (Ann a v) =
    if fragI == a ^. Property.pVal
    then
        fragmented ^. val & monoChildren %~ prepareInFragExpr
        & Ann ((HealPoint, 0), OnUnify (() <$ DataOps.replace a (fragmented ^. ann . Property.pVal)))
    else
        v & monoChildren %~ prepare fragI
        & Ann ((Other, 0), OnNoUnify (() <$ DataOps.applyHoleTo a))
    & fixPriorities
    where
        fragmented = v ^?! V._BApp . applyArg

inferBodies ::
    Tree UVar T.Type ->
    Tree (Ann a) Term ->
    PureInfer (Tree (Ann (a, Tree UVar T.Type, Tree UVar T.Type)) Term)
inferBodies extTyp (Ann a v) =
    v & monoChildren %~ childPre
    & inferBody
    >>=
    \(intTyp, infBody) ->
    monoChildren childPost infBody
    <&> Ann (a, extTyp, intTyp)
    where
        childPre x = Ann x (V.BLeaf V.LHole)
        childPost (ITerm x (IResult childTyp childScope) _) =
            inferBodies childTyp x
            & Reader.local (id .~ childScope)

tryUnify ::
    Monoid a =>
    (EditAction a, Tree UVar T.Type, Tree UVar T.Type) ->
    State InferState a
tryUnify (act, outTyp, inTyp) =
    do
        s0 <- get
        case runPureInfer V.emptyScope s0 (unify outTyp inTyp) of
            Left{} -> pure (act ^. _OnNoUnify)
            Right (t, s1) ->
                case runPureInfer V.emptyScope s1 (applyBindings t) of
                Left{} -> pure (act ^. _OnNoUnify)
                Right{} -> act ^. _OnUnify <$ put s1

healMismatch :: Monad m => ConvertM m (ValI m -> T m ())
healMismatch =
    do
        postProcess <- ConvertM.postProcessAssert
        topLevelExpr <-
            Lens.view ConvertM.scTopLevelExpr
            <&> annotations %~ (^. Input.stored)
        deps <- Lens.view (ConvertM.scFrozenDeps . Property.pVal)
        recursiveRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
        pure $
            \fragment ->
            let mPrep =
                    do
                        topVar <- newUnbound
                        let processInfer =
                                case recursiveRef of
                                Nothing -> id
                                Just rr ->
                                    V.scopeVarTypes . Lens.at (globalId (rr ^. ConvertM.rrDefI)) ?~ GMono topVar
                        addDeps <- loadDeps deps
                        prepare fragment topLevelExpr
                            & inferBodies topVar
                            & Reader.local (processInfer . addDeps)
                    & runPureInfer V.emptyScope (InferState emptyPureInferState varGen)
                (prep, inferState) = mPrep ^?! Lens._Right
            in
            prep ^.. annotations
            & sortOn (^. Lens._1 . Lens._1)
            <&> Lens._1 %~ (^. Lens._2)
            & traverse tryUnify
            & (`evalState` inferState)
            & sequence_
            >> postProcess
