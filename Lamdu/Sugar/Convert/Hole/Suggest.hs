{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( valueWith
    , valueConversion
    , stateMkVar
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Control.Monad.Trans.State (StateT(..))
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

stateMkVar :: Monad m => StateT Int m V.Var
stateMkVar =
    do
        i <- State.get
        State.modify (+1)
        "var" ++ show i & fromString & return

valueConversion ::
    (Monoid a, Applicative f, MonadA m) =>
    (T.Id -> m Nominal) ->
    f V.Var -> Val a -> Type -> Type -> m [f (Val a)]
valueConversion loadNominal mkVar arg (T.TInst name params) r =
    do
        fromNomType <-
            loadNominal name <&> Nominal.apply params
            -- TODO: Instantiate instead of access type?
            -- I think this happens to be fine for suggest but there are less
            -- doubts if using a proper instantiantion of the scheme..
            <&> (^. schemeType)
        valueConversionNoSplit mkVar fromNom fromNomType r
    <&> (: [pure fromNom])
    where
        fromNom = V.Nom name arg & V.BFromNom & V.Val mempty
valueConversion _ _ arg (T.TRecord composite) _ =
    composite ^.. ExprLens.compositeTags
    <&> pure . V.Val mempty . V.BGetField . V.GetField arg
    & return
valueConversion _ mkVar arg srcType dstType =
    valueConversionNoSplit mkVar arg srcType dstType
    <&> (:[])

valueConversionNoSplit ::
    (Monoid a, Applicative f, MonadA m) =>
    f V.Var -> Val a -> Type -> Type -> m (f (Val a))
valueConversionNoSplit mkVar arg (T.TSum composite) r =
    suggestCaseWith mkVar composite r
    <&> applyCase
    & return
    where
        applyCase c =
            c
            & Lens.traversed .~ mempty
            & (`V.Apply` arg) & V.BApp & V.Val mempty
valueConversionNoSplit _ _ _ _ = return $ pure P.hole

valueWith :: Applicative f => f V.Var -> Type -> [f (Val ())]
valueWith mkVar (T.TSum comp) =
    case comp of
    T.CVar{} -> [pure P.hole]
    _ ->
        comp ^.. ExprLens.compositeFields
        <&> \(tag, typ) ->
            valueWithNoSplit mkVar typ <&> P.inject tag
valueWith mkVar t = [valueWithNoSplit mkVar t]

valueWithNoSplit :: Applicative f => f V.Var -> Type -> f (Val ())
valueWithNoSplit mkVar (T.TRecord composite) =
    suggestRecordWith mkVar composite
valueWithNoSplit mkVar (T.TFun (T.TSum composite) r) =
    suggestCaseWith mkVar composite r
valueWithNoSplit mkVar (T.TFun _ r) =
    P.abs <$> mkVar <*> valueWithNoSplit mkVar r
valueWithNoSplit _ _ = pure P.hole

suggestRecordWith :: Applicative f => f V.Var -> T.Product -> f (Val ())
suggestRecordWith _ T.CVar{}          = pure P.hole
suggestRecordWith _ T.CEmpty          = pure P.recEmpty
suggestRecordWith mkVar (T.CExtend f t r) =
    P.recExtend f
    <$> valueWithNoSplit mkVar t
    <*> suggestRecordWith mkVar r

suggestCaseWith :: Applicative f => f V.Var -> T.Sum -> Type -> f (Val ())
suggestCaseWith _ T.CVar{} _ = pure P.hole
suggestCaseWith _ T.CEmpty _ = pure P.absurd
suggestCaseWith mkVar (T.CExtend f t r) res =
    P._case f
    <$> valueWithNoSplit mkVar (T.TFun t res)
    <*> suggestCaseWith mkVar r res
