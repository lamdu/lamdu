module Lamdu.Sugar.Convert.Fragment.Heal
    ( healMismatch
    ) where

import           AST (Tree, monoChildren)
import           AST.Infer (ITerm(..), IResult(..), infer, iType, iScope, irType)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.Apply (applyArg)
import           AST.Unify (unify, applyBindings, newUnbound)
import           AST.Unify.Binding (UVar)
import           AST.Unify.Generalize (GTerm(..))
import qualified Control.Lens.Extended as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (MonadState(..), State, execState)
import           Data.Property (Property)
import qualified Data.Property as Property
import           Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as Seq
import           Lamdu.Calc.Infer (runPureInfer, InferState(..), loadDeps, emptyPureInferState, varGen)
import qualified Lamdu.Calc.Lens as ExprLens
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

healSpine ::
    ValI m ->
    Tree (Ann (ValP m)) Term ->
    Tree (Ann (Maybe (Tree (Ann (ValP m)) Term))) Term
healSpine fragI tree
    | fragI == tree ^. ann . Property.pVal =
        tree ^?! val . V._BApp . applyArg & annotations .~ Nothing
    | Lens.has (monoChildren . ann . Lens._Nothing) treatedBody =
        Ann Nothing treatedBody
    | otherwise =
        Ann (Just tree) (V.BLeaf V.LHole)
        where
            treatedBody = tree ^. val & monoChildren %~ healSpine fragI

reconstructQueue ::
    Monad m =>
    State (InferState, Seq (Tree (Ann (ValP m)) Term, IResult UVar Term), [T m ()]) ()
reconstructQueue =
    popQueue >>=
    \case
    Nothing -> pure ()
    Just (Ann pos body, IResult var scope) ->
        do
            inferState0 <- Lens.use Lens._1
            let Right (inferredTop, inferState1) = runPureInfer scope inferState0 (infer top)
            case runPureInfer scope inferState1 (unify var (inferredTop ^. iType)) of
                Right (res, inferState2) | occursChecks ->
                    Lens._1 .= inferState2
                    where
                        occursChecks =
                            runPureInfer (inferredTop ^. iScope) inferState2 (applyBindings res)
                            & Lens.has Lens._Right
                _ ->
                    do
                        Lens._1 .= inferState1
                        Lens._3 %= ((() <$ DataOps.applyHoleTo pos) :)
            reconstructTerm inferredTop
        where
            top = body & monoChildren %~ (`Ann` V.BLeaf V.LHole) . Just & Ann Nothing
    where
        popQueue =
            do
                queue <- get
                case Seq.viewl queue of
                    EmptyL -> pure Nothing
                    x :< rest -> Just x <$ put rest
            & Lens.zoom Lens._2


reconstructTerm ::
    Monad m =>
    Tree (ITerm (Maybe (Tree (Ann (ValP m)) Term)) UVar) Term ->
    State (InferState, Seq (Tree (Ann (ValP m)) Term, IResult UVar Term), [T m ()]) ()
reconstructTerm iterm =
    do
        Lens._2 %= (>< newTodo)
        reconstructQueue
    where
        newTodo =
            iterm ^@..
            ExprLens.itermAnn . annotations . Lens.filteredBy (Lens._1 . Lens._Just) <. Lens._2
            & Seq.fromList

healMismatch :: Monad m => ConvertM m (Property (T m) (ValI m) -> ValI m -> T m ())
healMismatch =
    do
        postProcess <- ConvertM.postProcessAssert
        topLevelExpr <- Lens.view ConvertM.scTopLevelExpr
        deps <- Lens.view (ConvertM.scFrozenDeps . Property.pVal)
        recursiveRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
        pure $
            \fragment arg ->
            let mSpine =
                    do
                        processInfer <-
                            case recursiveRef of
                            Nothing -> pure id
                            Just rr ->
                                newUnbound
                                <&>
                                \defTv action ->
                                do
                                    res <-
                                        Reader.local
                                        (V.scopeVarTypes . Lens.at (globalId (rr ^. ConvertM.rrDefI)) ?~ GMono defTv)
                                        action
                                    res <$ unify defTv (res ^. iType)
                        addDeps <- loadDeps deps
                        healSpine (fragment ^. Property.pVal) (topLevelExpr & annotations %~ (^. Input.stored))
                            & infer
                            & processInfer
                            & Reader.local addDeps
                    & runPureInfer V.emptyScope (InferState emptyPureInferState varGen)
            in
            case mSpine of
            Right (spine, inferState) | occursChecks ->
                do
                    _ <- DataOps.replace fragment arg
                    sequence_ actions
                    postProcess
                where
                    (_, _, actions) = execState (reconstructTerm spine) (inferState, mempty, [])
                    occursChecks =
                        runPureInfer (spine ^. iScope) inferState
                        (traverse_ applyBindings (spine ^.. ExprLens.itermAnn . annotations . Lens._2 . irType))
                        & Lens.has Lens._Right
            _ -> pure () -- Cannot heal at all!
