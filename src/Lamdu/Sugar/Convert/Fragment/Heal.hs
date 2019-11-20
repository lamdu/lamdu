module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import qualified Control.Lens.Extended as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Property as Property
import           Hyper
import           Hyper.Infer.Blame (BlameResult(..), blame)
import qualified Hyper.Type.AST.Row as Row
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
    Annotated ((a, Int), b) Term ->
    Annotated ((a, Int), b) Term
fixPriorities x@(Ann (Const ((cat, priority), pl)) b) =
    case b of
    V.BGetField g ->
        g & V.getFieldRecord . score +~ (-1) & V.BGetField
        & res 1
    V.BRecExtend r -> V.BRecExtend r & res (-1)
    V.BCase c -> c & Row.eVal . score +~ (-1) & V.BCase & res (-1)
    V.BApp a -> a & V.appFunc . score +~ (-1) & V.BApp & res 0
    _ -> x
    where
        res diff = Ann (Const ((cat, priority + diff), pl))
        score = annotation . _1 . _2

prepareInFragExpr ::
    Monad m =>
    Annotated (ValP m) Term ->
    Annotated (Priority, EditAction (T m ())) Term
prepareInFragExpr (Ann (Const a) v) =
    v & htraverse1 %~ prepareInFragExpr
    & Ann (Const ((InFragment, 0), OnNoUnify (() <$ DataOps.applyHoleTo a)))
    & fixPriorities

prepare ::
    Monad m =>
    ValI m ->
    Annotated (ValP m) Term ->
    Annotated (Priority, EditAction (T m ())) Term
prepare fragI (Ann (Const a) v) =
    if fragI == a ^. Property.pVal
    then
        fragmented ^. hVal & htraverse1 %~ prepareInFragExpr
        & Ann (Const ((HealPoint, 0), OnUnify (() <$ DataOps.replace a (fragmented ^. annotation . Property.pVal))))
    else
        v & htraverse1 %~ prepare fragI
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
            <&> Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ (^. Input.stored)
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
                    & blame (^. Lens._Wrapped . Lens._1) (_ANode # topLevelType)
                    & Reader.local (addRecursiveRef . addDeps)
            & Infer.runPureInfer V.emptyScope
                (Infer.InferState Infer.emptyPureInferState Infer.varGen)
            & either (error "bug in heal!")
                (traverse_ act . (^.. Lens.from _HFlip . hfolded1) . fst)
            >> postProcess
    where
        act (Const (_, OnUnify x) :*: Good{}) = x
        act (Const (_, OnNoUnify x) :*: Mismatch{}) = x
        act _ = pure ()
