{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
    , valueNoSplit
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
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
    MonadA m =>
    a -> (T.Id -> m Nominal) ->
    Val (Type, a) -> Type -> m [Val (Type, a)]
valueConversion empty loadNominal src dstType =
    case src ^. V.payload . _1 of
    T.TRecord composite ->
        composite ^.. ExprLens.compositeFields
        <&> getField
        & return
        where
            getField (tag, typ) =
                V.GetField src tag & V.BGetField & V.Val (typ, empty)
    _ -> valueConversionNoSplit empty loadNominal src dstType <&> (:[])

valueConversionNoSplit ::
    MonadA m =>
    a -> (T.Id -> m Nominal) ->
    Val (Type, a) -> Type -> m (Val (Type, a))
valueConversionNoSplit empty loadNominal src dstType =
    case src ^. V.payload . _1 of
    T.TInst name params | bodyNot ExprLens._BToNom ->
        do
            fromNomType <-
                loadNominal name <&> Nominal.apply params
                -- TODO: Instantiate instead of access type?
                -- I think this happens to be fine for suggest but there are less
                -- doubts if using a proper instantiantion of the scheme..
                <&> (^. schemeType)
            let fromNom =
                    V.Nom name src & V.BFromNom & V.Val (fromNomType, empty)
            valueConversionNoSplit empty loadNominal fromNom dstType
    T.TFun argType resType | bodyNot ExprLens._BAbs ->
        valueConversionNoSplit empty loadNominal applied dstType
        where
            applied =
                valueNoSplit argType & Lens.traversed %~ flip (,) empty
                & V.Apply src & V.BApp & V.Val (resType, empty)
    T.TSum composite | bodyNot ExprLens._BInject  ->
        suggestCaseWith composite dstType & run
        & Lens.traversed %~ flip (,) empty
        & (`V.Apply` src) & V.BApp & V.Val (dstType, empty)
        & return
    _ -> return src
    where
        bodyNot f = Lens.nullOf (V.body . f) src

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
