{-# LANGUAGE TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
    ( convert
    ) where

import           Data.Property (Property(..))
import qualified Data.Property as Property
import           Hyper (Ann(..))
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
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
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Nominal as ConvertNominal
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

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
      ExprIRef.writeValI iref . V.BLeaf . V.LLiteral .
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
    ConvertM.PositionInfo -> Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
convert _ (Ann (Const pl) (V.BLam x)) = ConvertBinder.convertLam (Ann (Const pl) x)
convert _ (Ann (Const pl) (V.BRecExtend x)) = ConvertRecord.convertExtend (Ann (Const pl) x)
convert _ (Ann (Const pl) (V.BGetField x)) = ConvertGetField.convert (Ann (Const pl) x)
convert _ (Ann (Const pl) (V.BInject x)) = ConvertInject.convert (Ann (Const pl) x)
convert _ (Ann (Const pl) (V.BToNom x)) = ConvertNominal.convertToNom (Ann (Const pl) x)
convert _ (Ann (Const pl) (V.BCase x)) = ConvertCase.convert (Ann (Const pl) x)
convert posInfo (Ann (Const pl) (V.BApp x)) = ConvertApply.convert posInfo (Ann (Const pl) x)
convert posInfo (Ann (Const pl) (V.BLeaf l)) =
    pl &
    case l of
    V.LVar x -> ConvertGetVar.convert x
    V.LLiteral literal ->
        case PrimVal.toKnown literal of
        PrimVal.Float x -> convertLiteralFloat x
        PrimVal.Bytes x -> convertLiteralBytes x
    V.LHole -> ConvertHole.convert posInfo
    V.LRecEmpty -> ConvertRecord.convertEmpty
    V.LAbsurd -> ConvertCase.convertAbsurd
    V.LFromNom x -> ConvertNominal.convertFromNom x
