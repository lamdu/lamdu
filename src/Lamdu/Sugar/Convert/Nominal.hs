module Lamdu.Sugar.Convert.Nominal
    ( convertFromNom, convertToNom
    ) where

import           AST (Tree, Ann)
import           AST.Term.Nominal (ToNom(..))
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
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

convertNom ::
    Monad m =>
    NominalId -> expr -> ConvertM m (Nominal InternalName expr)
convertNom tid x =
    ConvertTId.convert tid
    <&> \tidS ->
    Nominal
    { _nTId = tidS
    , _nVal = x
    }

convertFromNom ::
    (Monad m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertFromNom (V.Nom tid x) pl =
    ConvertM.convertSubexpression x
    >>= convertNom tid <&> BodyFromNom
    >>= addActions [x] pl

convertToNom ::
    (Monad m, Monoid a) =>
    Tree (ToNom NominalId V.Term) (Ann (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom@(ToNom tid x) pl =
    do
        ConvertText.text nom pl & justToLeft
        ConvertBinder.convertBinder x
            >>= convertNom tid <&> BodyToNom
            >>= addActions [x] pl
            & lift
    & runMatcherT
