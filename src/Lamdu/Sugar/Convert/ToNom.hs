-- | Convert Text ToNoms to their own sugar construct
module Lamdu.Sugar.Convert.ToNom
     ( convert
     ) where

import qualified Control.Lens as Lens
import           Control.Monad (mzero)
import           Control.Monad.Trans.Except.Extended (justToLeft, runMatcherT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Extended (maybeToMPlus)
import           Data.Property (Property(..))
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Hyper.Syntax.Nominal (ToNom(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, PositionInfo(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    Monad m =>
    ToNom NominalId V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convert t@(ToNom tid x) pl =
    do
        text t pl & justToLeft
        Nominal
            <$> ConvertTId.convert tid
            <*> ConvertBinder.convertBinder BinderPos x
            <&> BodyToNom
            >>= addActions (Ann pl (V.BToNom t))
            & lift
    & runMatcherT
    <&> annotation . pActions . mApply .~ Nothing

text ::
    Monad m =>
    ToNom NominalId V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    MaybeT (ConvertM m) (ExpressionU v m)
text t@(ToNom tid (Ann litPl bod)) toNomPl =
    do
        Lens.view (ConvertM.scSugars . Config.literalText) >>= guard
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
                ExprIRef.writeValI litIRef . V.BLeaf . V.LLiteral .
                PrimVal.fromKnown . PrimVal.Bytes . encodeUtf8
            } & LiteralText & LeafLiteral & BodyLeaf & addActions (Ann toNomPl (V.BToNom t))
            & lift
    where
        litIRef = litPl ^. Input.stored . ExprIRef.iref
