module Lamdu.Sugar.Convert.Hole
    ( convert
    ) where

import           Hyper
import qualified Lamdu.Calc.Term as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) =>
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convert holePl =
    addActions (Const ()) holePl (BodyLeaf LeafHole)
    <&> annotation . pActions . delete .~ CannotDelete
    <&> annotation . pActions . mApply .~ Nothing
