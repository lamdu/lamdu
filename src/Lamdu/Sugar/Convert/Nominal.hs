module Lamdu.Sugar.Convert.Nominal
    ( convertFromNom, convertToNom
    ) where

import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertNom :: Monad m => V.Nom expr -> ConvertM m (Nominal InternalName expr)
convertNom (V.Nom tid val) =
    ConvertTId.convert tid
    <&> \tidS ->
    Nominal
    { _nTId = tidS
    , _nVal = val
    }

convertFromNom ::
    (Monad m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertFromNom nom pl =
    traverse ConvertM.convertSubexpression nom
    >>= convertNom <&> BodyFromNom
    >>= addActions nom pl

convertToNom ::
    (Monad m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom pl =
    do
        ConvertText.text nom pl & justToLeft
        traverse ConvertBinder.convertBinderBody nom
            >>= convertNom <&> BodyToNom
            >>= addActions nom pl
            & lift
    & runMatcherT
