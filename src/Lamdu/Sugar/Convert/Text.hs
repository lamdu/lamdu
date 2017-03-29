-- | Convert Text ToNoms to their own sugar construct
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Text
     ( text
     ) where

import           Control.Monad (mzero)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

text ::
    (Monad m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
text (V.Nom tid (Val litPl body)) toNomPl =
    do
        guard $ tid == Builtins.textTid
        lit <- body ^? ExprLens.valBodyLiteral & maybeToMPlus
        utf8Bytes <-
            case PrimVal.toKnown lit of
            PrimVal.Bytes utf8Bytes -> return utf8Bytes
            _ -> mzero
        Property
            { _pVal = decodeUtf8 utf8Bytes
            , _pSet =
                ExprIRef.writeValBody litIRef . V.BLeaf . V.LLiteral .
                PrimVal.fromKnown . PrimVal.Bytes . encodeUtf8
            } & LiteralText & BodyLiteral & addActions toNomPl
            <&> rPayload . plData <>~ litPl ^. Input.userData
            & lift
    where
        litIRef = litPl ^. Input.stored . Property.pVal
