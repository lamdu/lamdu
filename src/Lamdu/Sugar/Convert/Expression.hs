{-# LANGUAGE FlexibleContexts, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
    ( convert
    ) where

import           Data.Property (Property(..))
import qualified Data.Property as Property
import           Data.Tree.Diverse (ann, val)
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
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
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

convertLiteralCommon ::
    (Monad m, Monoid b) =>
    (Property (T m) a -> Literal (Property (T m))) ->
    (a -> PrimVal.KnownPrim) -> a ->
    Input.Payload m b -> ConvertM m (ExpressionU m b)
convertLiteralCommon mkLit mkBody x exprPl =
    Property
    { _pVal = x
    , _pSet =
      Transaction.writeIRef iref . V.BLeaf . V.LLiteral .
      PrimVal.fromKnown . mkBody
    } & mkLit & BodyLiteral & addActions [] exprPl
    where
        iref = exprPl ^. Input.stored . Property.pVal

convertLiteralFloat ::
    (Monad m, Monoid a) =>
    Double -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLiteralFloat = convertLiteralCommon LiteralNum PrimVal.Float

convertLiteralBytes ::
    (Monad m, Monoid a) =>
    ByteString -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLiteralBytes = convertLiteralCommon LiteralBytes PrimVal.Bytes

convert ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
convert v =
    v ^. ann
    & case v ^. val of
      V.BLam x -> ConvertBinder.convertLam x
      V.BApp x -> ConvertApply.convert x
      V.BRecExtend x -> ConvertRecord.convertExtend x
      V.BGetField x -> ConvertGetField.convert x
      V.BInject x -> ConvertInject.convert x
      V.BToNom x -> ConvertNominal.convertToNom x
      V.BFromNom x -> ConvertNominal.convertFromNom x
      V.BCase x -> ConvertCase.convert x
      V.BLeaf (V.LVar x) -> ConvertGetVar.convert x
      V.BLeaf (V.LLiteral literal) ->
          case PrimVal.toKnown literal of
          PrimVal.Float x -> convertLiteralFloat x
          PrimVal.Bytes x -> convertLiteralBytes x
      V.BLeaf V.LHole -> ConvertHole.convert
      V.BLeaf V.LRecEmpty -> ConvertRecord.convertEmpty
      V.BLeaf V.LAbsurd -> ConvertCase.convertAbsurd
