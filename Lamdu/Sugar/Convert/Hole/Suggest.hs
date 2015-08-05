{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
    , valueNoSplit
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Control.Monad.Trans.State (State, evalState)
import qualified Control.Monad.Trans.State as State
import           Data.String (IsString(..))
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Nominal as Nominal
import           Lamdu.Expr.Scheme (schemeType)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

-- For fresh variable names:
type M = State Int
run :: M a -> a
run act = evalState act 0

mkVar :: M V.Var
mkVar =
    do
        i <- State.get
        State.modify (+1)
        "var" ++ show i & fromString & return

valueConversion ::
    (Monoid a, MonadA m) =>
    (T.Id -> m Nominal) ->
    Val a -> Type -> Type -> m [Val a]
valueConversion _ arg (T.TRecord composite) _ =
    composite ^.. ExprLens.compositeTags
    <&> V.Val mempty . V.BGetField . V.GetField arg
    & return
valueConversion loadNominal arg srcType dstType =
    valueConversionNoSplit loadNominal arg srcType dstType
    <&> (:[])

valueConversionNoSplit ::
    (Monoid a, MonadA m) =>
    (T.Id -> m Nominal) -> Val a -> Type -> Type -> m (Val a)
valueConversionNoSplit loadNominal arg (T.TInst name params) r =
    do
        fromNomType <-
            loadNominal name <&> Nominal.apply params
            -- TODO: Instantiate instead of access type?
            -- I think this happens to be fine for suggest but there are less
            -- doubts if using a proper instantiantion of the scheme..
            <&> (^. schemeType)
        valueConversionNoSplit loadNominal fromNom fromNomType r
    where
        fromNom = V.Nom name arg & V.BFromNom & V.Val mempty
valueConversionNoSplit loadNominal arg (T.TFun at rt) r =
    valueConversionNoSplit loadNominal applied rt r
    where
        applied =
            valueNoSplit at & Lens.traversed .~ mempty
            & V.Apply arg & V.BApp & V.Val mempty
valueConversionNoSplit _ arg (T.TSum composite) r =
    suggestCaseWith composite r & run
    & Lens.traversed .~ mempty
    & (`V.Apply` arg) & V.BApp & V.Val mempty
    & return
valueConversionNoSplit _ arg _ _ = return arg

value :: Type -> [Val Type]
value typ@(T.TSum comp) =
    case comp of
    T.CVar{} -> [V.BLeaf V.LHole]
    _ -> comp ^.. ExprLens.compositeFields <&> inject
    <&> Val typ
    where
        inject (tag, innerTyp) =
            valueNoSplitM innerTyp & run & V.Inject tag & V.BInject
value typ = [valueNoSplitM typ & run]

valueNoSplit :: Type -> Val Type
valueNoSplit typ = valueNoSplitM typ & run

valueNoSplitM :: Type -> M (Val Type)
valueNoSplitM (T.TRecord composite) = suggestRecordWith composite
valueNoSplitM (T.TFun (T.TSum composite) r) = suggestCaseWith composite r
valueNoSplitM typ =
    case typ of
    T.TFun _ r -> V.Lam <$> mkVar <*> valueNoSplitM r <&> V.BAbs
    _ -> V.BLeaf V.LHole & pure
    <&> Val typ

suggestRecordWith :: T.Product -> M (Val Type)
suggestRecordWith recordType =
    case recordType of
    T.CVar{}        -> V.BLeaf V.LHole & pure
    T.CEmpty        -> V.BLeaf V.LRecEmpty & pure
    T.CExtend f t r ->
        V.RecExtend f
        <$> valueNoSplitM t
        <*> suggestRecordWith r
        <&> V.BRecExtend
    <&> Val (T.TRecord recordType)

suggestCaseWith :: T.Sum -> Type -> M (Val Type)
suggestCaseWith sumType resultType =
    case sumType of
    T.CVar{} -> V.BLeaf V.LHole & pure
    T.CEmpty -> V.BLeaf V.LAbsurd & pure
    T.CExtend tag fieldType rest ->
        V.Case tag
        <$> valueNoSplitM (T.TFun fieldType resultType)
        <*> suggestCaseWith rest resultType
        <&> V.BCase
    <&> Val (T.TFun (T.TSum sumType) resultType)
