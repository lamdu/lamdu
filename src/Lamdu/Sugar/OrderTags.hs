{-# LANGUAGE TypeApplications, FlexibleInstances, MultiParamTypeClasses, DefaultSignatures, ScopedTypeVariables, UndecidableInstances #-}

module Lamdu.Sugar.OrderTags
    ( orderDef, orderType, orderNode
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>))
import           Control.Monad.Transaction (MonadTransaction(..), setP)
import           Data.List (sortOn)
import           Hyper
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Data.Tag (tagOrder)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type OrderT m x = x -> m x

class Order i t where
    order :: OrderT i (t # Annotated a)

    default order ::
        ( MonadTransaction m i, HTraversable t
        , HNodesConstraint t (Order i)
        ) =>
        OrderT i (t # Annotated a)
    order = htraverse (Proxy @(Order i) #> orderNode)

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

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Composite v name i o) where
    order (Sugar.Composite items punned tail_ addItem) =
        Sugar.Composite
        <$> (orderByTag (^. Sugar.ciTag . Sugar.tagRefTag) items
            >>= (traverse . Sugar.ciExpr) orderNode)
        <*> pure punned
        <*> Sugar._OpenComposite orderNode tail_
        <*> pure addItem

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.LabeledApply v name i o) where
    order (Sugar.LabeledApply func specialArgs annotated punned) =
        Sugar.LabeledApply func specialArgs
        <$> orderByTag (^. Sugar.aaTag) annotated
        <*> pure punned
        >>= htraverse (Proxy @(Order i) #> orderNode)

instance MonadTransaction m i => Order i (Const a)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Else v name i o)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.IfElse v name i o)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Let v name i o)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.PostfixApply v name i o)

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Lambda v name i o) where
    order = Sugar.lamFunc order

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Function v name i o) where
    order x =
        x
        & (Sugar.fParams . Sugar._Params) orderParams
        >>= Sugar.fBody orderNode
        <&> Sugar.fParams . Sugar._Params %~ addReorders

tagChoiceOptions ::
    Functor i =>
    Lens.Setter
    (Sugar.TagChoice n0 i o a) (Sugar.TagChoice n1 i o a)
    (Sugar.TagOption n0 o a) (Sugar.TagOption n1 o a)
tagChoiceOptions =
    Lens.setting (\f (Sugar.TagChoice o n a) -> Sugar.TagChoice (o <&> traverse %~ f) (n <&> f) a)

tagChoicePick :: Functor i => Lens.IndexedSetter' T.Tag (Sugar.TagChoice n i o a) (o a)
tagChoicePick = tagChoiceOptions . Lens.filteredBy (Sugar.toInfo . Sugar.tagVal) <. Sugar.toPick

addReorders ::
    (MonadTransaction m o, Functor i) =>
    [(a, Sugar.ParamInfo n i o)] -> [(a, Sugar.ParamInfo n i o)]
addReorders params =
    params & Lens.itraversed <. _2 %@~ addParamActions
    where
        tags = params ^.. traverse . _2 . Sugar.piTag . Sugar.tagRefTag . Sugar.tagVal
        addParamActions ::
            (MonadTransaction m o, Functor i) =>
            Int -> Sugar.ParamInfo n i o -> Sugar.ParamInfo n i o
        addParamActions i a =
            a
            & Sugar.piTag . Sugar.tagRefReplace . tagChoicePick %@~
                (\t ->
                    (transaction (
                        ExprIRef.readTagData (a ^. Sugar.piTag . Sugar.tagRefTag . Sugar.tagVal)
                        <&> (^. tagOrder) >>= DataOps.setTagOrder t) >>))
            & Sugar.piActions . Sugar.fpAddNext . Sugar._AddNext . tagChoicePick %@~
                (\t -> (transaction (Lens.itraverse_ (flip DataOps.setTagOrder) (before <> [t] <> after)) >>))
            & Sugar.piActions . Sugar.fpMOrderBefore .~
                (setOrder ([0..i-1] <> [i, i-1] <> [i+1..length tags-1]) <$ guard (i > 0))
            & Sugar.piActions . Sugar.fpMOrderAfter .~
                (setOrder ([0..i] <> [i+1, i] <> [i+2..length tags-1]) <$ guard (i + 1 < length tags))
            where
                (before, after) = splitAt (i+1) tags
        setOrder :: MonadTransaction m o => [Int] -> o ()
        setOrder o =
            Lens.itraverse_ (flip DataOps.setTagOrder) (o <&> \i -> tags ^?! Lens.ix i)
            & transaction

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.PostfixFunc v name i o) where
    order (Sugar.PfCase x) = order x <&> Sugar.PfCase
    order x@Sugar.PfFromNom{} = pure x
    order x@Sugar.PfGetField{} = pure x

orderParams ::
    MonadTransaction m i =>
    OrderT i [(Sugar.FuncParam v name, Sugar.ParamInfo name i o)]
orderParams = orderByTag (^. _2 . Sugar.piTag . Sugar.tagRefTag)

-- Special case assignment and binder to invoke the special cases in expr

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Assignment v name i o) where
    order (Sugar.BodyPlain x) = Sugar.apBody order x <&> Sugar.BodyPlain
    order (Sugar.BodyFunction x) = order x <&> Sugar.BodyFunction

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Binder v name i o) where
    order (Sugar.BinderTerm x) = order x <&> Sugar.BinderTerm
    order (Sugar.BinderLet x) = order x <&> Sugar.BinderLet

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Term v name i o) where
    order (Sugar.BodyLam l) = order l <&> Sugar.BodyLam
    order (Sugar.BodyRecord r) = order r <&> Sugar.BodyRecord
    order (Sugar.BodyLabeledApply a) = order a <&> Sugar.BodyLabeledApply
    order (Sugar.BodyPostfixFunc f) = order f <&> Sugar.BodyPostfixFunc
    order (Sugar.BodyFragment a) =
        a
        & Sugar.fExpr orderNode
        <&> Sugar.BodyFragment
    order (Sugar.BodyIfElse x) = order x <&> Sugar.BodyIfElse
    order (Sugar.BodyToNom x) = Sugar.nVal orderNode x <&> Sugar.BodyToNom
    order (Sugar.BodySimpleApply x) = htraverse1 orderNode x <&> Sugar.BodySimpleApply
    order (Sugar.BodyPostfixApply x) = order x <&> Sugar.BodyPostfixApply
    order (Sugar.BodyNullaryInject x) = Sugar.BodyNullaryInject x & pure
    order (Sugar.BodyLeaf x) =
        x
        & Sugar._LeafHole . Sugar.holeOptions . Lens.mapped . Lens.mapped
            %~ (>>= (traverse . Sugar.optionExpr) orderNode)
        & Sugar.BodyLeaf
        & pure

orderNode ::
    (MonadTransaction m i, Order i f) =>
    OrderT i (Annotated a # f)
orderNode = hVal order

orderDef ::
    (MonadTransaction m o, MonadTransaction m i) =>
    OrderT i (Sugar.Definition v name i o a)
orderDef def =
    def
    & (SugarLens.defSchemes . Sugar.schemeType) orderType
    >>= (Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent) orderNode
    <&> Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent .
        hVal . Sugar._BodyFunction . Sugar.fParams . Sugar._Params %~
        setVerboseWhenNeeded 2 Sugar.fpMOrderAfter . setVerboseWhenNeeded 3 Sugar.fpMOrderBefore
    where
        setVerboseWhenNeeded c l =
            Lens.taking c traverse . _2 . Sugar.piActions . l . Lens._Just %~ (setToVerbose >>)
        setToVerbose = setP (Anchors.assocPresentationMode (def ^. Sugar.drDefI)) Sugar.Verbose
