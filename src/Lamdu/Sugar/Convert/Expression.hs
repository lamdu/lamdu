module Lamdu.Sugar.Convert.Expression
    ( convert
    ) where

import           Control.Monad.Once (Typeable)
import           Data.Property (Property(..))
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Apply as Apply
import qualified Lamdu.Sugar.Convert.Binder as Binder
import qualified Lamdu.Sugar.Convert.Case as Case
import qualified Lamdu.Sugar.Convert.GetField as GetField
import qualified Lamdu.Sugar.Convert.GetVar as GetVar
import qualified Lamdu.Sugar.Convert.Hole as Hole
import qualified Lamdu.Sugar.Convert.Inject as Inject
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, PositionInfo)
import           Lamdu.Sugar.Convert.NodeActions (addActions)
import qualified Lamdu.Sugar.Convert.Record as Record
import qualified Lamdu.Sugar.Convert.ToNom as ToNom
import qualified Lamdu.Sugar.Convert.TId as TId
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

convertFromNom :: Monad m => NominalId -> Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertFromNom tid pl =
    TId.convert tid <&> PfFromNom <&> BodyPostfixFunc >>= addActions (Ann pl (V.BLeaf (V.LFromNom tid)))

convert ::
    (Monad m, Typeable m) =>
    PositionInfo ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convert pos (Ann pl (V.BLam x)) = Binder.convertLam pos x pl
convert _ (Ann pl (V.BRecExtend x)) = Record.convertExtend x pl
convert _ (Ann pl (V.BToNom x)) = ToNom.convert x pl
convert _ (Ann pl (V.BCase x)) = Case.convert x pl
convert _ (Ann pl (V.BApp x)) = Apply.convert x pl
convert posInfo (Ann pl (V.BLeaf l)) =
    pl &
    case l of
    V.LVar x -> GetVar.convert x
    V.LLiteral literal ->
        case PrimVal.toKnown literal of
        PrimVal.Float x -> convertLiteralFloat x
        PrimVal.Bytes x -> convertLiteralBytes x
        PrimVal.Char x -> convertLiteralChar x
    V.LHole -> Hole.convert posInfo
    V.LRecEmpty -> Record.convertEmpty
    V.LAbsurd -> Case.convertAbsurd
    V.LFromNom x -> convertFromNom x
    V.LGetField x -> GetField.convert x
    V.LInject x -> Inject.convert (BodyLeaf . LeafInject) x
