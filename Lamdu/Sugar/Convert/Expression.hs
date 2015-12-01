{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
    ( convert
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Data.Binary.Utils (decodeS)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
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
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Nominal as ConvertNominal
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

jumpToDefI ::
    MonadA m => Anchors.CodeProps m -> DefI m -> T m EntityId
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newPane cp defI

convertLiteralFloat ::
    MonadA m => Double ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLiteralFloat i exprPl = addActions exprPl $ BodyLiteralNum i

convertGlobal ::
    MonadA m => V.GlobalId -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertGlobal globalId exprPl =
    do
        cp <- (^. ConvertM.scCodeAnchors) <$> ConvertM.readContext
        addActions exprPl .
            BodyGetVar $ GetBinder BinderVar
            { _bvNameRef = NameRef
              { _nrName = UniqueId.toGuid defI
              , _nrGotoDefinition = jumpToDefI cp defI
              }
            , _bvForm = GetDefinition
            , _bvMInline = Nothing
            }
    where
        defI = ExprIRef.defI globalId

convertGetVar ::
    MonadA m =>
    V.Var -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertGetVar param exprPl =
    do
        sugarContext <- ConvertM.readContext
        ConvertGetVar.convertVar sugarContext param
            (exprPl ^. Input.inferredType)
            & BodyGetVar
            & addActions exprPl

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
      V.BLeaf (V.LVar x) -> convertGetVar x
      V.BLeaf (V.LGlobal x) -> convertGlobal x
      V.BLeaf (V.LLiteral (V.Literal p bs))
          | p == Builtins.floatId -> convertLiteralFloat (decodeS bs)
          | otherwise -> error "TODO Literals which are not nums"
      V.BLeaf V.LHole -> ConvertHole.convert
      V.BLeaf V.LRecEmpty -> ConvertRecord.convertEmpty
      V.BLeaf V.LAbsurd -> ConvertCase.convertAbsurd
