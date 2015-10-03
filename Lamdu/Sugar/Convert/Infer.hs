{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Infer
    ( loadInfer
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Lamdu.Builtins.Anchors (recurseVar)
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import           Prelude.Compat

type T = Transaction

loadInfer ::
    MonadA m =>
    CurAndPrev (EvalResults (ExprIRef.ValI m)) ->
    Val (ExprIRef.ValIProperty m) ->
    T m (Either IRefInfer.Error (Val (Input.Payload m ()), Infer.Context))
loadInfer evalRes val =
    IRefInfer.loadInferRecursive recurseVar val
    <&> Lens.mapped %~ mkPayload
    >>= ParamList.loadForLambdas
    & IRefInfer.run
    where
        mkPayload (inferPl, valIProp) =
            Input.mkPayload () inferPl
            (evalRes <&> exprEvalRes execId)
            valIProp
            where
                execId = Property.value valIProp
        exprEvalRes pl r =
            Input.EvalResultsForExpr
            (r ^. erExprValues . Lens.at pl . Lens._Just)
            (r ^. erAppliesOfLam . Lens.at pl . Lens._Just)
