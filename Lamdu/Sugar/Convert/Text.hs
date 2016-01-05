-- | Convert Text ToNoms to their own sugar construct
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Text
     ( text
     ) where

import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Prelude.Compat

text ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
text (V.Nom tid (Val litPl body)) toNomPl =
    do
        guard $ tid == Builtins.textTid
        V.Literal litTag utf8Bytes <- body ^? ExprLens.valBodyLiteral & maybeToMPlus
        guard $ litTag == Builtins.bytesId
        UTF8.toString utf8Bytes
            & LiteralText & BodyLiteral & addActions toNomPl
            <&> rPayload . plData <>~ litPl ^. Input.userData
            & lift
