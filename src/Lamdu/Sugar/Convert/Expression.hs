module Lamdu.Sugar.Convert.Expression
    ( convert
    ) where

import           Control.Monad.Once (Typeable)
import           Data.Property (Property(..))
import qualified Lamdu.Builtins.PrimVal as PrimVal
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
import           Lamdu.Sugar.Convert.Monad (ConvertM, PositionInfo)
import qualified Lamdu.Sugar.Convert.Nominal as ConvertNominal
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convertLiteralCommon ::
    Monad m =>
    (Property (T m) a -> Literal (Property (T m))) ->
    (a -> PrimVal.KnownPrim) -> a ->
    Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertLiteralCommon mkLit mkBody x exprPl =
    Property
    { _pVal = x
    , _pSet = ExprIRef.writeValI iref . bod
    } & mkLit & LeafLiteral & BodyLeaf & addActions (Ann exprPl (bod x))
    <&> annotation . pActions . mApply .~ Nothing
    where
        bod = V.BLeaf . V.LLiteral . PrimVal.fromKnown . mkBody
        iref = exprPl ^. Input.stored . ExprIRef.iref

convertLiteralFloat :: Monad m => Double -> Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertLiteralFloat = convertLiteralCommon LiteralNum PrimVal.Float

convertLiteralBytes :: Monad m => ByteString -> Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertLiteralBytes = convertLiteralCommon LiteralBytes PrimVal.Bytes

convertLiteralChar :: Monad m => Char -> Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertLiteralChar = convertLiteralCommon LiteralChar PrimVal.Char

convert ::
    (Monad m, Typeable m) =>
    PositionInfo ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convert pos (Ann pl (V.BLam x)) = ConvertBinder.convertLam pos x pl
convert _ (Ann pl (V.BRecExtend x)) = ConvertRecord.convertExtend x pl
convert _ (Ann pl (V.BToNom x)) = ConvertNominal.convertToNom x pl
convert _ (Ann pl (V.BCase x)) = ConvertCase.convert x pl
convert _ (Ann pl (V.BApp x)) = ConvertApply.convert x pl
convert posInfo (Ann pl (V.BLeaf l)) =
    pl &
    case l of
    V.LVar x -> ConvertGetVar.convert x
    V.LLiteral literal ->
        case PrimVal.toKnown literal of
        PrimVal.Float x -> convertLiteralFloat x
        PrimVal.Bytes x -> convertLiteralBytes x
        PrimVal.Char x -> convertLiteralChar x
    V.LHole -> ConvertHole.convert posInfo
    V.LRecEmpty -> ConvertRecord.convertEmpty
    V.LAbsurd -> ConvertCase.convertAbsurd
    V.LFromNom x -> ConvertNominal.convertFromNom x
    V.LGetField x -> ConvertGetField.convert x
    V.LInject x -> ConvertInject.convert (BodyLeaf . LeafInject) x
