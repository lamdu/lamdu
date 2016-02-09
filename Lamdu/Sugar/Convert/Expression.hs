{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
    ( convert
    ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.ByteString as SBS
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Builtins.Literal (Lit(..))
import qualified Lamdu.Builtins.Literal as BuiltinLiteral
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Case as ConvertCase
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.GetField as ConvertGetField
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Inject as ConvertInject
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Nominal as ConvertNominal
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Prelude.Compat

convertLiteralCommon ::
    Monad m =>
    (Transaction.Property m a -> Literal (Transaction.Property m)) ->
    (a -> Lit) -> a ->
    Input.Payload m b -> ConvertM m (ExpressionU m b)
convertLiteralCommon mkLit mkBody val exprPl =
    Property
    { _pVal = val
    , _pSet =
      ExprIRef.writeValBody iref . V.BLeaf . V.LLiteral .
      BuiltinLiteral.fromLit . mkBody
    } & mkLit & BodyLiteral & addActions exprPl
    where
        iref = exprPl ^. Input.stored . Property.pVal

convertLiteralFloat ::
    MonadA m => Double -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLiteralFloat = convertLiteralCommon LiteralNum LitFloat

convertLiteralBytes ::
    MonadA m => SBS.ByteString -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLiteralBytes = convertLiteralCommon LiteralBytes LitBytes

convert :: (MonadA m, Monoid a) => Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
convert v =
    v ^. V.payload
    & case v ^. V.body of
      V.BAbs x -> ConvertBinder.convertLam x
      V.BApp x -> ConvertApply.convert x
      V.BRecExtend x -> ConvertRecord.convertExtend x
      V.BGetField x -> ConvertGetField.convert x
      V.BInject x -> ConvertInject.convert x
      V.BToNom x -> ConvertNominal.convertToNom x
      V.BFromNom x -> ConvertNominal.convertFromNom x
      V.BCase x -> ConvertCase.convert x
      V.BLeaf (V.LVar x) -> ConvertGetVar.convert x
      V.BLeaf (V.LLiteral literal) ->
          case BuiltinLiteral.toLit literal of
          LitFloat x -> convertLiteralFloat x
          LitBytes x -> convertLiteralBytes x
      V.BLeaf V.LHole -> ConvertHole.convert
      V.BLeaf V.LRecEmpty -> ConvertRecord.convertEmpty
      V.BLeaf V.LAbsurd -> ConvertCase.convertAbsurd
