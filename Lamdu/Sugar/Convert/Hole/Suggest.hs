{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
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
import qualified Lamdu.Expr.Pure as P
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
valueConversion loadNominal arg (T.TInst name params) r =
    do
        fromNomType <-
            loadNominal name <&> Nominal.apply params
            -- TODO: Instantiate instead of access type?
            -- I think this happens to be fine for suggest but there are less
            -- doubts if using a proper instantiantion of the scheme..
            <&> (^. schemeType)
        valueConversionNoSplit fromNom fromNomType r
    <&> (: [fromNom])
    where
        fromNom = V.Nom name arg & V.BFromNom & V.Val mempty
valueConversion _ arg (T.TRecord composite) _ =
    composite ^.. ExprLens.compositeTags
    <&> V.Val mempty . V.BGetField . V.GetField arg
    & return
valueConversion _ arg srcType dstType =
    valueConversionNoSplit arg srcType dstType
    <&> (:[])

valueConversionNoSplit ::
    (Monoid a, MonadA m) => Val a -> Type -> Type -> m (Val a)
valueConversionNoSplit arg (T.TSum composite) r =
    suggestCaseWith composite r & run & applyCase & return
    where
        applyCase c =
            c
            & Lens.traversed .~ mempty
            & (`V.Apply` arg) & V.BApp & V.Val mempty
valueConversionNoSplit _ _ _ = return P.hole

value :: Type -> [Val ()]
value (T.TSum comp) =
    case comp of
    T.CVar{} -> [P.hole]
    _ ->
        comp ^.. ExprLens.compositeFields
        <&> \(tag, typ) -> valueNoSplit typ & run & P.inject tag
value t = [valueNoSplit t & run]

valueNoSplit :: Type -> M (Val ())
valueNoSplit (T.TRecord composite) =
    suggestRecordWith composite
valueNoSplit (T.TFun (T.TSum composite) r) =
    suggestCaseWith composite r
valueNoSplit (T.TFun _ r) =
    P.abs <$> mkVar <*> valueNoSplit r
valueNoSplit _ = pure P.hole

suggestRecordWith :: T.Product -> M (Val ())
suggestRecordWith T.CVar{}          = pure P.hole
suggestRecordWith T.CEmpty          = pure P.recEmpty
suggestRecordWith (T.CExtend f t r) =
    P.recExtend f
    <$> valueNoSplit t
    <*> suggestRecordWith r

suggestCaseWith :: T.Sum -> Type -> M (Val ())
suggestCaseWith T.CVar{} _ = pure P.hole
suggestCaseWith T.CEmpty _ = pure P.absurd
suggestCaseWith (T.CExtend f t r) res =
    P._case f
    <$> valueNoSplit (T.TFun t res)
    <*> suggestCaseWith r res
