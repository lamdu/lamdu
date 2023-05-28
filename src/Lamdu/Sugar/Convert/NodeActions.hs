module Lamdu.Sugar.Convert.NodeActions
    ( addActions, makeApply
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens.Extended as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Hyper
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.NodeActions.Extract (makeExtract)
import           Lamdu.Sugar.Convert.NodeActions.ReplaceParent (setChildReplaceParentActions)
import           Lamdu.Sugar.Convert.NodeActions.SetToLiteral (makeSetToLiteral)
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

makeActions ::
    Monad m =>
    Input.Payload m # V.Term -> ConvertM m (NodeActions (T m))
makeActions exprPl =
    do
        ext <- makeExtract exprPl
        postProcess <- ConvertM.postProcessAssert
        setToLit <- makeSetToLiteral exprPl
        apply <- makeApply stored
        pure NodeActions
            { _detach = DataOps.applyHoleTo stored <* postProcess <&> EntityId.ofValI & DetachAction
            , _delete = DataOps.setToHole stored <* postProcess <&> EntityId.ofValI & SetToHole
            , _setToLiteral = setToLit
            , _extract = ext
            , _mReplaceParent = Nothing
            , _mApply = Just apply
            }
    where
        stored = exprPl ^. Input.stored

makeApply :: Monad m => ExprIRef.HRef m # V.Term -> ConvertM m (T m EntityId)
makeApply stored =
    Lens.view ConvertM.scPostProcessRoot
    <&> \checkOk ->
    do
        tryApp checkOk noop noop
            <|> tryApp checkOk wrap noop -- prefer wrapping outside
            <|> tryApp checkOk noop wrap -- then wrapping inside
            <|> tryApp checkOk wrap wrap -- then both
            & runMaybeT
            <&> fromMaybe (error "Failed to type-check apply with fragments in&out of it")
        <&> EntityId.ofValI
    where
        tryApp checkOk outside inside =
            do
                holeArg <- V.BLeaf V.LHole & ExprIRef.newValI
                thing <- inside (stored ^. ExprIRef.iref)
                V.App thing holeArg & V.BApp & ExprIRef.newValI
                    >>= outside
                    >>= stored ^. ExprIRef.setIref
                pure holeArg
                & ConvertM.typeProtect checkOk
                & MaybeT
        noop :: Monad f => ExprIRef.ValI f -> T f (ExprIRef.ValI f)
        noop = pure
        wrap :: Monad f => ExprIRef.ValI f -> T f (ExprIRef.ValI f)
        wrap iref = V.App <$> DataOps.newHole ?? iref <&> V.BApp >>= ExprIRef.newValI

addActions ::
    Monad m =>
    Ann (Input.Payload m) # V.Term ->
    Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m) ->
    ConvertM m (ExpressionU v m)
addActions expr bodyS =
    do
        actions <- makeActions (expr ^. hAnn)
        addReplaceParents <- setChildReplaceParentActions
        Ann
            { _hVal = addReplaceParents (expr ^. hAnn . Input.stored) bodyS
            , _hAnn =
                Const ConvertPayload
                { _pUnsugared = expr
                , _pActions = actions
                , _pLambdas = []
                , _pEntityId = expr ^. hAnn . Input.entityId
                }
            } & pure
