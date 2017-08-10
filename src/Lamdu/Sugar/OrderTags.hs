{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.Sugar.OrderTags
    ( orderDef, orderType, orderExpr
    , orderedFlatComposite, orderedClosedFlatComposite
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Utils (tagged)
import           Control.Monad ((>=>))
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Anchors (assocTagOrder)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.FlatComposite (FlatComposite(..))
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import qualified Lamdu.Calc.Type.Scheme as S
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type Order m x = x -> Transaction m x

orderByTag :: Monad m => (a -> T.Tag) -> Order m [a]
orderByTag toTag =
    fmap (map fst . sortOn snd) . mapM loadOrder
    where
        loadOrder x =
            toTag x
            & assocTagOrder
            & Transaction.getP
            <&> (,) x

orderComposite :: Monad m => Order m (T.Composite p)
orderComposite c =
    fields
    & Map.toList
    & orderByTag fst
    <&> foldr (uncurry T.CExtend) (maybe T.CEmpty T.CVar mExt)
    >>= ExprLens.compositeTypes orderType
    where
        FlatComposite fields mExt = FlatComposite.fromComposite c

orderType :: Monad m => Order m Type
orderType t =
    t
    & T._TRecord %%~ orderComposite
    >>= T._TSum %%~ orderComposite
    >>= ExprLens.nextLayer orderType

orderRecord :: Monad m => Order m (Sugar.Record name f a)
orderRecord = Sugar.rItems %%~ orderByTag (^. Sugar.rfTag . Sugar.tagVal)

orderLabeledApply :: Monad m => Order m (Sugar.LabeledApply name binderVar a)
orderLabeledApply = Sugar.aAnnotatedArgs %%~ orderByTag (^. Sugar.aaTag . Sugar.tagVal)

orderHoleResult :: Monad m => Order m (Sugar.HoleResult name m)
orderHoleResult = Sugar.holeResultConverted %%~ orderExpr

orderHole :: Monad m => Sugar.Hole name m a -> Sugar.Hole name m a
orderHole =
    Sugar.holeActions . Sugar.holeOptions . Lens.mapped . Lens.mapped .
    Sugar.hoResults . Lens.mapped . Lens._2 %~ (>>= orderHoleResult)

orderCase :: Monad m => Order m (Sugar.Case name m a)
orderCase = Sugar.cAlts %%~ orderByTag (^. Sugar.caTag . Sugar.tagVal)

orderLam :: Monad m => Order m (Sugar.Lambda name m a)
orderLam = Sugar.lamBinder orderBinder

orderBody :: Monad m => Order m (Sugar.Body name m a)
orderBody (Sugar.BodyLam l) = orderLam l <&> Sugar.BodyLam
orderBody (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
orderBody (Sugar.BodyLabeledApply a) = orderLabeledApply a <&> Sugar.BodyLabeledApply
orderBody (Sugar.BodyCase c) = orderCase c <&> Sugar.BodyCase
orderBody (Sugar.BodyHole a) = orderHole a & Sugar.BodyHole & return
orderBody x@Sugar.BodyGuard{} = return x
orderBody x@Sugar.BodySimpleApply{} = return x
orderBody x@Sugar.BodyLiteral{} = return x
orderBody x@Sugar.BodyGetField{} = return x
orderBody x@Sugar.BodyGetVar{} = return x
orderBody x@Sugar.BodyInject{} = return x
orderBody x@Sugar.BodyToNom{} = return x
orderBody x@Sugar.BodyFromNom{} = return x
orderBody x@Sugar.BodyInjectedExpression{} = return x

orderExpr :: Monad m => Order m (Sugar.Expression name m a)
orderExpr e =
    e
    & Sugar.rPayload . Sugar.plAnnotation . Sugar.aInferredType %%~ orderType
    >>= Sugar.rBody %%~ orderBody
    >>= Sugar.rBody . Lens.traversed %%~ orderExpr

orderParams :: Monad m => Order m [(T.Tag, Sugar.FuncParam info)]
orderParams xs =
    xs
    & Lens.traversed . _2 . Sugar.fpAnnotation . Sugar.aInferredType %%~ orderType
    >>= orderByTag (^. _1)

orderBinder ::
    Monad m => Order m (Sugar.Binder name m a)
orderBinder b =
    b
    & Sugar.bParams . Sugar._FieldParams %%~ orderParams

orderDef ::
    Monad m => Order m (Sugar.Definition name m (Sugar.Expression name m a))
orderDef def =
    def
    & SugarLens.defSchemes . S.schemeType %%~ orderType
    >>= Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent
        %%~ (orderBinder >=> Lens.traversed %%~ orderExpr)

{-# INLINE orderedFlatComposite #-}
orderedFlatComposite ::
    Lens.Iso (T.Composite a) (T.Composite b)
    ([(T.Tag, T.Type)], Maybe (T.Var (T.Composite a)))
    ([(T.Tag, T.Type)], Maybe (T.Var (T.Composite b)))
orderedFlatComposite =
    Lens.iso to from
    where
        to T.CEmpty = ([], Nothing)
        to (T.CVar x) = ([], Just x)
        to (T.CExtend tag typ rest) = to rest & Lens._1 %~ (:) (tag, typ)
        from ([], Nothing) = T.CEmpty
        from ([], Just x) = T.CVar x
        from ((tag,typ):rest, v) = (rest, v) & from & T.CExtend tag typ

orderedClosedFlatComposite :: Lens.Prism' (T.Composite b) [(T.Tag, Type)]
orderedClosedFlatComposite =
    orderedFlatComposite . tagged Lens._Nothing
