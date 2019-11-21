module Lamdu.Sugar.Convert.Record
    ( convertEmpty, convertExtend
    ) where

import qualified Control.Lens as Lens
import           Hyper (Ann(..), annotation)
import           Hyper.Combinator.Ann (Annotated)
import           Hyper.Type.AST.Row (RowExtend(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Composite as Composite
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . ExprIRef.iref

convertEmpty ::
    (Monad m, Monoid a) => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertEmpty pl =
    Composite.convertEmpty DataOps.recExtend pl
    <&> BodyRecord
    >>= addActions [] pl

convertExtend ::
    (Monad m, Monoid a) =>
    Annotated (Input.Payload m a) (RowExtend T.Tag V.Term V.Term) ->
    ConvertM m (ExpressionU m a)
convertExtend (Ann (Const exprPl) (RowExtend tag val rest)) =
    do
        valS <- ConvertM.convertSubexpression val
        restS <- ConvertM.convertSubexpression rest
        let recP =
                Composite.ExtendVal
                { Composite._extendTag = tag
                , Composite._extendValI = val ^. annotation . plValI
                , Composite._extendRest = rest ^. annotation
                }
        Composite.convert DataOps.recExtend V.LRecEmpty mkRecExtend _BodyRecord valS restS exprPl recP
    where
        mkRecExtend t v r = RowExtend t v r & V.BRecExtend
