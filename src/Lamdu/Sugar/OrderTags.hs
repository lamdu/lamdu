{-# LANGUAGE TypeApplications, FlexibleInstances, MultiParamTypeClasses, DefaultSignatures, ScopedTypeVariables, UndecidableInstances #-}

module Lamdu.Sugar.OrderTags
    ( orderDef, orderType, orderNode
    ) where

import           Control.Monad ((>=>))
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.List (sortOn)
import           Hyper
import           Lamdu.Data.Tag (tagOrder)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type OrderT m x = x -> m x

class Order v name i o t where
    order :: OrderT i (t # Annotated (Sugar.Payload v name i o, a))

    default order ::
        ( MonadTransaction m i, HTraversable t
        , HNodesConstraint t (Order v name i o)
        ) =>
        OrderT i (t # Annotated (Sugar.Payload v name i o, a))
    order = htraverse (Proxy @(Order v name i o) #> orderNode)

orderByTag :: MonadTransaction m i => (a -> Sugar.Tag name) -> OrderT i [a]
orderByTag toTag =
    fmap (map fst . sortOn snd) . traverse loadOrder
    where
        loadOrder x =
            toTag x ^. Sugar.tagVal
            & ExprIRef.readTagData
            & transaction
            <&> (,) x . (^. tagOrder)

orderComposite ::
    MonadTransaction m i =>
    OrderT i (Sugar.CompositeFields name (Ann a # Sugar.Type name))
orderComposite = Sugar.compositeFields (orderByTag fst >=> (traverse . _2) orderType)

orderTBody ::
    MonadTransaction m i =>
    OrderT i (Sugar.Type name # Ann a)
orderTBody t =
    t
    & Sugar._TRecord %%~ orderComposite
    >>= Sugar._TVariant %%~ orderComposite
    >>= htraverse1 orderType

orderType :: MonadTransaction m i => OrderT i (Ann a # Sugar.Type name)
orderType = hVal orderTBody

instance MonadTransaction m i => Order v name i o (Sugar.Composite v name i o) where
    order (Sugar.Composite items punned tail_ addItem) =
        Sugar.Composite
        <$> (orderByTag (^. Sugar.ciTag . Sugar.tagRefTag) items
            >>= (traverse . Sugar.ciExpr) orderNode)
        <*> pure punned
        <*> (Sugar._OpenComposite . _2) orderNode tail_
        <*> pure addItem

instance MonadTransaction m i => Order v name i o (Sugar.LabeledApply v name i o) where
    order (Sugar.LabeledApply func specialArgs annotated punned) =
        Sugar.LabeledApply func specialArgs
        <$> orderByTag (^. Sugar.aaTag) annotated
        <*> pure punned
        >>= htraverse (Proxy @(Order v name i o) #> orderNode)

instance MonadTransaction m i => Order v name i o (Sugar.Lambda v name i o)
instance MonadTransaction m i => Order v name i o (Const a)
instance MonadTransaction m i => Order v name i o (Sugar.Else v name i o)
instance MonadTransaction m i => Order v name i o (Sugar.IfElse v name i o)
instance MonadTransaction m i => Order v name i o (Sugar.Let v name i o)
instance MonadTransaction m i => Order v name i o (Sugar.PostfixApply v name i o)

instance MonadTransaction m i => Order v name i o (Sugar.Function v name i o) where
    order x =
        x
        & (Sugar.fParams . Sugar._Params) orderParams
        >>= Sugar.fBody orderNode

instance MonadTransaction m i => Order v name i o (Sugar.PostfixFunc v name i o) where
    order (Sugar.PfCase x) = order x <&> Sugar.PfCase
    order x@Sugar.PfFromNom{} = pure x
    order x@Sugar.PfGetField{} = pure x

orderParams ::
    MonadTransaction m i =>
    OrderT i [(Sugar.FuncParam v name, Sugar.ParamInfo name i o)]
orderParams = orderByTag (^. _2 . Sugar.piTag . Sugar.tagRefTag)

-- Special case assignment and binder to invoke the special cases in expr

instance MonadTransaction m i => Order v name i o (Sugar.Assignment v name i o) where
    order (Sugar.BodyPlain x) = Sugar.apBody order x <&> Sugar.BodyPlain
    order (Sugar.BodyFunction x) = order x <&> Sugar.BodyFunction

instance MonadTransaction m i => Order v name i o (Sugar.Binder v name i o) where
    order (Sugar.BinderTerm x) = order x <&> Sugar.BinderTerm
    order (Sugar.BinderLet x) = order x <&> Sugar.BinderLet

instance MonadTransaction m i => Order v name i o (Sugar.Term v name i o) where
    order (Sugar.BodyLam l) = order l <&> Sugar.BodyLam
    order (Sugar.BodyRecord r) = order r <&> Sugar.BodyRecord
    order (Sugar.BodyLabeledApply a) = order a <&> Sugar.BodyLabeledApply
    order (Sugar.BodyPostfixFunc f) = order f <&> Sugar.BodyPostfixFunc
    order (Sugar.BodyHole a) = SugarLens.holeTransformExprs orderNode a & Sugar.BodyHole & pure
    order (Sugar.BodyFragment a) =
        a
        & Sugar.fOptions %~ SugarLens.holeTransformExprs orderNode
        & Sugar.fExpr orderNode
        <&> Sugar.BodyFragment
    order (Sugar.BodyIfElse x) = order x <&> Sugar.BodyIfElse
    order (Sugar.BodyToNom x) = Sugar.nVal orderNode x <&> Sugar.BodyToNom
    order (Sugar.BodySimpleApply x) = htraverse1 orderNode x <&> Sugar.BodySimpleApply
    order (Sugar.BodyPostfixApply x) = order x <&> Sugar.BodyPostfixApply
    order x@Sugar.BodyInject{} = pure x
    order x@Sugar.BodyLiteral{} = pure x
    order x@Sugar.BodyGetVar{} = pure x
    order x@Sugar.BodyPlaceHolder{} = pure x

orderNode ::
    (MonadTransaction m i, Order v name i o f) =>
    OrderT i (Annotated (Sugar.Payload v name i o, a) # f)
orderNode = hVal order

orderDef ::
    MonadTransaction m i =>
    OrderT i (Sugar.Definition v name i o (Sugar.Payload v name i o, a))
orderDef def =
    def
    & (SugarLens.defSchemes . Sugar.schemeType) orderType
    >>= (Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent) orderNode
