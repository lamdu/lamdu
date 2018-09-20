-- | Convert Text ToNoms to their own sugar construct
module Lamdu.Sugar.Convert.Text
     ( text
     ) where

import           Control.Monad (mzero)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Extended (maybeToMPlus)
import           Data.Property (Property(..))
import qualified Data.Property as Property
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.Tree.Diverse (Ann(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

text ::
    (Monad m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
text n@(V.Nom tid (Ann litPl bod)) toNomPl =
    do
        guard $ tid == Builtins.textTid
        lit <- bod ^? ExprLens.valBodyLiteral & maybeToMPlus
        txt <-
            case PrimVal.toKnown lit of
            PrimVal.Bytes utf8Bytes ->
                case decodeUtf8' utf8Bytes of
                Right txt -> pure txt
                Left{} -> mzero
            _ -> mzero
        Property
            { _pVal = txt
            , _pSet =
                Transaction.writeIRef litIRef . V.BLeaf . V.LLiteral .
                PrimVal.fromKnown . PrimVal.Bytes . encodeUtf8
            } & LiteralText & BodyLiteral & addActions n toNomPl
            & lift
    where
        litIRef = litPl ^. Input.stored . Property.pVal
