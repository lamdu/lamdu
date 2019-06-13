module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import           AST (Tree, monoChildren)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.Apply (applyArg)
import qualified AST.Term.Row as Row
import           AST.Unify.Generalize (GTerm(..))
import qualified Control.Lens.Extended as Lens
import qualified Data.Property as Property
import           Lamdu.Calc.Infer (loadDeps)
import           Lamdu.Calc.Infer.Refragment (refragment)
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Calc.Term as V
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
    V.BApp a -> a & V.applyFunc . score +~ (-1) & V.BApp & res 0
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

healMismatch :: Monad m => ConvertM m (ValI m -> T m ())
healMismatch =
    do
        postProcess <- ConvertM.postProcessAssert
        topLevelExpr <-
            Lens.view ConvertM.scTopLevelExpr
            <&> annotations %~ (^. Input.stored)
        deps <- Lens.view (ConvertM.scFrozenDeps . Property.pVal)
        recursiveRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
        let addRecursiveRef topVar =
                case recursiveRef of
                Nothing -> id
                Just rr ->
                    V.scopeVarTypes . Lens.at (globalId (rr ^. ConvertM.rrDefI)) ?~ GMono topVar
        let prepareInfer topVar = loadDeps deps <&> (addRecursiveRef topVar .)
        pure $
            \fragment ->
            prepare fragment topLevelExpr
            & refragment fst act prepareInfer
            & sequence_
            >> postProcess
    where
        act True (_, OnUnify x) = x
        act False (_, OnNoUnify x) = x
        act _ _ = pure ()
