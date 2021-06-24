module Lamdu.Sugar.Convert.Nominal
    ( convertToNom, convertFromNom
    ) where

import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Hyper (_ANode)
import           Hyper.Syntax.Nominal (ToNom(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertToNom ::
    (Monad m, Monoid a) =>
    ToNom NominalId V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convertToNom t@(ToNom tid x) pl =
    do
        ConvertText.text t pl & justToLeft
        Nominal
            <$> ConvertTId.convert tid
            <*> ConvertBinder.convertBinder x
            <&> BodyToNom
            >>= addActions (_ANode # x) pl
            & lift
    & runMatcherT
    <&> annotation . pActions . mApply .~ Nothing

convertFromNom ::
    (Monad m, Monoid a) =>
    NominalId -> Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU v m a)
convertFromNom tid pl =
    ConvertTId.convert tid <&> PfFromNom <&> BodyPostfixFunc >>= addActions (Const ()) pl
