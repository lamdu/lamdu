module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import qualified Control.Lens.Extended as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Property as Property
import           Hyper (Tree, htraverse1)
import           Hyper.Infer.Blame (blame, bTermToAnn)
import qualified Hyper.Type.AST.Row as Row
import           Hyper.Type.Ann (Ann(..), ann, val, annotations)
import           Hyper.Type.Combinator.Flip (Flip(..))
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.New (newUnbound)
import qualified Lamdu.Calc.Infer as Infer
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, ValP, globalId)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data EditAction a
    = OnUnify a
    | OnNoUnify a

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
    V.BApp a -> a & V.appFunc . score +~ (-1) & V.BApp & res 0
    _ -> x
    where
        res diff = Ann ((cat, priority + diff), pl)
        score = ann . _1 . _2

prepareInFragExpr ::
    Monad m =>
    Tree (Ann (ValP m)) Term ->
    Tree (Ann (Priority, EditAction (T m ()))) Term
prepareInFragExpr (Ann a v) =
    v & htraverse1 %~ prepareInFragExpr
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
        fragmented ^. val & htraverse1 %~ prepareInFragExpr
        & Ann ((HealPoint, 0), OnUnify (() <$ DataOps.replace a (fragmented ^. ann . Property.pVal)))
    else
        v & htraverse1 %~ prepare fragI
        & Ann ((Other, 0), OnNoUnify (() <$ DataOps.applyHoleTo a))
    & fixPriorities
    where
        fragmented = v ^?! V._BApp . V.appArg

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
            do
                topLevelType <- newUnbound
                let addRecursiveRef =
                        case recursiveRef of
                        Nothing -> id
                        Just rr ->
                            V.scopeVarTypes .
                            Lens.at (globalId (rr ^. ConvertM.rrDefI)) ?~
                            MkFlip (GMono topLevelType)
                addDeps <- Infer.loadDeps deps
                prepare fragment topLevelExpr
                    & blame (^. Lens._1) (V.IResult V.emptyScope topLevelType)
                    & Reader.local (addRecursiveRef . addDeps)
            & Infer.runPureInfer V.emptyScope
                (Infer.InferState Infer.emptyPureInferState Infer.varGen)
            & either (error "bug in heal!")
                (Lens.sequenceOf_ annotations .
                    bTermToAnn act .
                    fst
                )
            >> postProcess
    where
        act _ (_, OnUnify x) Right{} = x
        act _ (_, OnNoUnify x) Left{} = x
        act _ _ _ = pure ()
