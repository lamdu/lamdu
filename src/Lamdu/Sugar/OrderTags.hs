{-# LANGUAGE TypeApplications, FlexibleInstances, MultiParamTypeClasses, DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

module Lamdu.Sugar.OrderTags
    ( orderWorkArea, orderType
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>), zipWithM_)
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Property (mkProperty, pVal, pSet)
import           Data.List (sortOn, elemIndex)
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

orderByTag :: MonadTransaction m i => [T.Tag] -> (a -> Sugar.Tag name) -> (a -> i b) -> [a] -> i [b]
orderByTag presModeTags toTag ord =
    fmap (map fst . sortOn snd) . traverse f
    where
        f x = (,) <$> ord x <*> getOrder (toTag x ^. Sugar.tagVal)
        getOrder t =
            case elemIndex t presModeTags of
            Just x -> Left x & pure
            Nothing -> ExprIRef.readTagData t & transaction <&> Right

orderComposite ::
    MonadTransaction m i =>
    OrderT i (Sugar.CompositeFields name (Ann a # Sugar.Type name o))
orderComposite = Sugar.compositeFields (orderByTag [] fst (_2 orderType))

orderTBody ::
    MonadTransaction m i =>
    OrderT i (Sugar.Type name o # Ann a)
orderTBody t =
    t
    & Sugar._TRecord %%~ orderComposite
    >>= Sugar._TVariant %%~ orderComposite
    >>= htraverse1 orderType

orderType :: MonadTransaction m i => OrderT i (Ann a # Sugar.Type name o)
orderType = hVal orderTBody

orderTaggedList ::
    (MonadTransaction m f, MonadTransaction n o, Functor i) =>
    [T.Tag] -> (a -> f a) -> Sugar.TaggedList name i o a -> f (Sugar.TaggedList name i o a)
orderTaggedList presMode orderItem (Sugar.TaggedList addFirst items) =
    Sugar.TaggedList addFirst <$> Lens._Just (orderTaggedListBody presMode orderItem) items
    <&> addReorders

orderTaggedListBody ::
    (MonadTransaction m f, Applicative o) =>
    [T.Tag] -> (a -> f a) -> Sugar.TaggedListBody name i o a -> f (Sugar.TaggedListBody name i o a)
orderTaggedListBody presMode orderItem tlb =
    orderByTag presMode (^. Sugar.tiTag . Sugar.tagRefTag) (Sugar.tiValue orderItem) (tlb ^.. SugarLens.taggedListBodyItems) <&>
    \(newHd : newTl) ->
    newTl <&> (`Sugar.TaggedSwappableItem` pure ())
    & Sugar.TaggedListBody newHd

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Composite v name i o) where
    order (Sugar.Composite items punned tail_) =
        Sugar.Composite
        <$> orderTaggedList [] orderNode items
        <*> pure punned
        <*> Sugar._OpenComposite orderNode tail_

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.LabeledApply v name i o) where
    order (Sugar.LabeledApply func specialArgs annotated punned) =
        Sugar.LabeledApply func
        <$> (Lens._Just . htraverse1) orderNode specialArgs
        <*> orderByTag [] (^. Sugar.aaTag) (Sugar.aaExpr orderNode) annotated
        ?? punned

instance MonadTransaction m i => Order i (Const a)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Else v name i o)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.IfElse v name i o)
instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.PostfixApply v name i o)

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Let v name i o) where
    order l =
        l
        & htraverse (Proxy @(Order i) #> orderNode)
        >>= (Sugar.lNames . Sugar._LhsRecord) (orderTaggedList [] pure)

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Lambda v name i o) where
    order = Sugar.lamFunc order

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Function v name i o) where
    order (Sugar.Function chosenScope params body bodyScopes) =
        Sugar.Function chosenScope
        <$> Sugar._LhsRecord (orderTaggedList [] pure) params
        <*> orderNode body
        ?? bodyScopes

tagChoicePick :: Lens.IndexedSetter' T.Tag (Sugar.TagChoice n o) (o ())
tagChoicePick = SugarLens.tagChoiceOptions . Lens.filteredBy (Sugar.toInfo . Sugar.tagVal) <. Sugar.toPick

addReorders :: (MonadTransaction m o, Functor i) => Sugar.TaggedList n i o a -> Sugar.TaggedList n i o a
addReorders tl =
    tl
    & Sugar.tlAddFirst . Lens.mapped . tagChoicePick %@~ fixPrepend
    & Lens.indexing SugarLens.taggedListItems %@~ fixItem
    & Sugar.tlItems . Lens._Just . Sugar.tlTail . Lens.traversed <. Sugar.tsiSwapWithPrevious .@~ mkSwap
    where
        tags = tl ^.. SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagVal
        fixPrepend tag = (fixOrders (tag : tags) *>)
        fixItem idx =
            (Sugar.tiTag . Sugar.tagRefReplace . Lens.mapped . tagChoicePick %@~ \n -> (fixOrders (pre <> (n : post)) *>)) .
            (Sugar.tiAddAfter . Lens.mapped . tagChoicePick %@~ \n -> (fixOrders (pre <> (cur : n : post)) *>))
            where
                (pre, cur : post) = splitAt idx tags
        mkSwap idx =
            fixOrders (pre <> (y : x : post))
            where
                (pre, x : y : post) = splitAt idx tags

-- Change sequence so that each item will be larger than previous.
fixRisingSequence :: [Int] -> [Int]
fixRisingSequence s =
    -- TODO: smart algorithm that does minimal changes to sequence
    enumFromTo 0 (length s - 1)

fixOrders :: MonadTransaction m o => [T.Tag] -> o ()
fixOrders tags =
    traverse ExprIRef.readTagData tags <&> (^.. traverse . tagOrder)
    <&> fixRisingSequence
    >>= zipWithM_ DataOps.setTagOrder tags
    & transaction

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.PostfixFunc v name i o) where
    order (Sugar.PfCase x) = order x <&> Sugar.PfCase
    order x@Sugar.PfFromNom{} = pure x
    order x@Sugar.PfGetField{} = pure x

-- Special case assignment and binder to invoke the special cases in expr

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Assignment v name i o) where
    order (Sugar.BodyPlain x) = Sugar.apBody order x <&> Sugar.BodyPlain
    order (Sugar.BodyFunction x) = order x <&> Sugar.BodyFunction

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Binder v name i o) where
    order = Sugar.bBody order

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.BinderBody v name i o) where
    order (Sugar.BinderTerm x) = order x <&> Sugar.BinderTerm
    order (Sugar.BinderLet x) = order x <&> Sugar.BinderLet

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.Term v name i o) where
    order (Sugar.BodyLam l) = order l <&> Sugar.BodyLam
    order (Sugar.BodyRecord r) = order r <&> Sugar.BodyRecord
    order (Sugar.BodyLabeledApply a) = order a <&> Sugar.BodyLabeledApply
    order (Sugar.BodyPostfixFunc f) = order f <&> Sugar.BodyPostfixFunc
    order (Sugar.BodyFragment a) =
        a
        & Sugar.fOptions . Lens.mapped . Lens.mapped
            %~ (>>= (traverse . Sugar.optionExpr) orderNode)
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

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.FragOpt v name i o) where
    order (Sugar.FragArgument a) = order a <&> Sugar.FragArgument
    order x = pure x

instance (MonadTransaction m o, MonadTransaction m i) => Order i (Sugar.HoleOpt v name i o) where
    order (Sugar.HoleBinder x) = order x <&> Sugar.HoleBinder
    order (Sugar.HoleVarsRecord f) = Sugar.HoleVarsRecord f & pure -- TODO: Sort fields!

orderNode ::
    (MonadTransaction m i, Order i f) =>
    OrderT i (Annotated a # f)
orderNode = hVal order

setVerboseWhenNeeded :: (Functor i, Applicative o) => o () -> Sugar.TaggedListBody name i o a -> Sugar.TaggedListBody name i o a
setVerboseWhenNeeded setVerbose t =
    Sugar.TaggedListBody
    { Sugar._tlHead = t ^. Sugar.tlHead & onTaggedItem
    , Sugar._tlTail =
        t ^. Sugar.tlTail
        & Lens.ix 0 %~ (Sugar.tsiItem %~ onTaggedItem) . (Sugar.tsiSwapWithPrevious %~ (setVerbose *>))
        & Lens.ix 1 . Sugar.tsiSwapWithPrevious %~ (setVerbose *>)
    }
    where
        onTaggedItem =
            (Sugar.tiDelete %~ (setVerbose *>)) .
            (SugarLens.taggedItemTagChoices . SugarLens.tagChoiceOptions . Sugar.toPick %~ (setVerbose *>))

orderDef ::
    (MonadTransaction m o, MonadTransaction m i) =>
    OrderT i (Sugar.Definition v name i o a)
orderDef def =
    def
    & (SugarLens.defSchemes . Sugar.schemeType) orderType
    >>= (Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent)
        (orderNode >=> (hVal . Sugar._BodyFunction . Sugar.fParams . Sugar._LhsRecord) processPresentationMode)
    where
        processPresentationMode orig =
            do
                presModeProp <- Anchors.assocPresentationMode (def ^. Sugar.drDefI) ^. mkProperty & transaction
                let setToVerbose = (presModeProp ^. pSet) Sugar.Verbose & transaction
                orderTaggedList (presModeProp ^.. pVal . Sugar._Operator . Lens.both) pure orig
                    <&> Sugar.tlItems . Lens._Just %~ setVerboseWhenNeeded setToVerbose

orderPaneBody :: (MonadTransaction m o, MonadTransaction m i) => OrderT i (Sugar.PaneBody v name i o a)
orderPaneBody (Sugar.PaneDefinition x) = orderDef x <&> Sugar.PaneDefinition
orderPaneBody (Sugar.PaneNominal x) =
    Sugar.npParams (orderTaggedList [] pure) x
    >>= (Sugar.npBody . Lens._Just . Sugar.schemeType) orderType
    <&> Sugar.PaneNominal
orderPaneBody x@Sugar.PaneTag{} = pure x

orderWorkArea :: (MonadTransaction m o, MonadTransaction m i) => OrderT i (Sugar.WorkArea v name i o a)
orderWorkArea (Sugar.WorkArea panes repl globs) =
    Sugar.WorkArea
    <$> (traverse . Sugar.paneBody) orderPaneBody panes
    <*> Sugar.replExpr orderNode repl
    ?? globs
