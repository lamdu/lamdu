-- | Convert Text ToNoms to their own sugar construct
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Text
     ( text
     ) where

import           Control.Lens.Operators
import           Control.Monad (guard, mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Maybe.Utils (maybeToMPlus)
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

import           Prelude.Compat

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
            { _pVal = UTF8.toString utf8Bytes
            , _pSet =
                ExprIRef.writeValBody litIRef . V.BLeaf . V.LLiteral .
                PrimVal.fromKnown . PrimVal.Bytes . UTF8.fromString
            } & LiteralText & BodyLiteral & addActions toNomPl
            <&> rPayload . plData <>~ litPl ^. Input.userData
            & lift
    where
        litIRef = litPl ^. Input.stored . Property.pVal
