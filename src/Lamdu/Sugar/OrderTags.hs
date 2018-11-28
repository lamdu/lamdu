{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Lamdu.Sugar.OrderTags
    ( orderDef, orderType, orderBinder
    , orderedClosedFlatComposite
    ) where

import           AST (Node)
import           AST.Ann (Ann(..), ann, val)
import qualified Control.Lens.Extended as Lens
import           Data.List (sortOn)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Tag (tagOrder)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction
type OrderT m x = x -> T m x

class Order m name o t where
    order :: OrderT m (t (Ann (Sugar.Payload name i o a)))

orderByTag :: Monad m => (a -> Sugar.TagInfo name) -> OrderT m [a]
orderByTag toTag =
    fmap (map fst . sortOn snd) . traverse loadOrder
    where
        loadOrder x =
            toTag x ^. Sugar.tagVal
            & ExprIRef.readTagInfo
            <&> (,) x . (^. tagOrder)

orderComposite :: Monad m => OrderT m (Sugar.CompositeFields p name (Sugar.Type a))
orderComposite =
    Sugar.compositeFields $
    \fields -> fields & orderByTag (^. _1) >>= traverse . _2 %%~ orderType

orderTBody :: Monad m => OrderT m (Sugar.TBody name (Sugar.Type name))
orderTBody t =
    t
    & Sugar._TRecord %%~ orderComposite
    >>= Sugar._TVariant %%~ orderComposite
    >>= traverse orderType

orderType :: Monad m => OrderT m (Sugar.Type name)
orderType = Sugar.tBody %%~ orderTBody

orderRecord ::
    Monad m =>
    OrderT m (Sugar.Composite name (T m) o (Sugar.Expression name (T m) o (Sugar.Payload name i o a)))
orderRecord r =
    r
    & Sugar.cItems (orderByTag (^. Sugar.ciTag . Sugar.tagInfo))
    >>= traverse orderExpr

instance Monad m => Order m name o (Sugar.LabeledApply name (T m) o) where
    order a =
        a
        & Sugar.aAnnotatedArgs %%~ orderByTag (^. Sugar.aaTag)
        >>= SugarLens.labeledApplyChildren pure pure orderExpr

orderCase ::
    Monad m =>
    OrderT m (Sugar.Case name (T m) o (Sugar.Expression name (T m) o (Sugar.Payload name i o a)))
orderCase = Sugar.cBody %%~ orderRecord

instance Monad m => Order m name o (Sugar.Lambda name (T m) o) where
    order = Sugar.lamFunc order

instance Monad m => Order m name o (Sugar.Binder name (T m) o) where
    order (Sugar.BinderExpr x) = order x <&> Sugar.BinderExpr
    order (Sugar.BinderLet x) =
        x
        & Sugar.lBody orderBinder
        >>= Sugar.lValue orderAssignment
        <&> Sugar.BinderLet

orderBinder ::
    Monad m =>
    OrderT m (Node (Ann (Sugar.Payload name i o a)) (Sugar.Binder name (T m) o))
orderBinder = val order

instance Monad m => Order m name o (Sugar.Else name (T m) o) where
    order (Sugar.SimpleElse x) = order x <&> Sugar.SimpleElse
    order (Sugar.ElseIf x) = Sugar.eiContent order x <&> Sugar.ElseIf

instance Monad m => Order m name o (Sugar.IfElse name (T m) o) where
    order x =
        x
        & Sugar.iIf orderExpr
        >>= Sugar.iThen orderExpr
        >>= (Sugar.iElse . val) order

instance Monad m => Order m name o (Sugar.Body name (T m) o) where
    order (Sugar.BodyLam l) = order l <&> Sugar.BodyLam
    order (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
    order (Sugar.BodyLabeledApply a) = order a <&> Sugar.BodyLabeledApply
    order (Sugar.BodyCase c) = orderCase c <&> Sugar.BodyCase
    order (Sugar.BodyHole a) = SugarLens.holeTransformExprs orderBinder a & Sugar.BodyHole & pure
    order (Sugar.BodyFragment a) =
        a
        & Sugar.fOptions . Lens.mapped . Lens.mapped %~ SugarLens.holeOptionTransformExprs orderBinder
        & Sugar.BodyFragment
        & pure
    order (Sugar.BodyIfElse x) = order x <&> Sugar.BodyIfElse
    order (Sugar.BodyInject x) = (Sugar.iContent . Sugar._InjectVal) orderExpr x <&> Sugar.BodyInject
    order (Sugar.BodyToNom x) = traverse orderBinder x <&> Sugar.BodyToNom
    order (Sugar.BodyFromNom x) = traverse orderExpr x <&> Sugar.BodyFromNom
    order (Sugar.BodySimpleApply x) = traverse orderExpr x <&> Sugar.BodySimpleApply
    order (Sugar.BodyGetField x) = traverse orderExpr x <&> Sugar.BodyGetField
    order x@Sugar.BodyLiteral{} = pure x
    order x@Sugar.BodyGetVar{} = pure x
    order x@Sugar.BodyPlaceHolder{} = pure x

orderExpr ::
    Monad m => OrderT m (Sugar.Expression name (T m) o (Sugar.Payload name i o a))
orderExpr e =
    e
    & ann . Sugar.plAnnotation . SugarLens.annotationTypes %%~ orderType
    >>= val %%~ order
    -- TODO: Skipping ordering of "if"
    >>= val (SugarLens.bodyChildren pure pure pure pure orderBinder orderExpr)

instance Monad m => Order m name o (Sugar.Function name (T m) o) where
    order =
        -- The ordering for binder params already occurs at the Assignment's conversion,
        -- because it needs to be consistent with the presentation mode.
        Sugar.fBody orderBinder

instance Monad m => Order m name o (Sugar.AssignmentBody name (T m) o) where
    order (Sugar.BodyFunction x) = order x <&> Sugar.BodyFunction
    order (Sugar.BodyPlain x) = Sugar.apBody order x <&> Sugar.BodyPlain

orderAssignment :: Monad m => OrderT m (Sugar.Assignment name (T m) o (Sugar.Payload name i o a))
orderAssignment = val order

orderDef ::
    Monad m => OrderT m (Sugar.Definition name (T m) o (Sugar.Payload name i o a))
orderDef def =
    def
    & (SugarLens.defSchemes . Sugar.schemeType) orderType
    >>= (Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent) orderAssignment

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

orderedClosedFlatComposite :: Lens.Prism' (T.Composite b) [(T.Tag, T.Type)]
orderedClosedFlatComposite = orderedFlatComposite . Lens.tagged Lens._Nothing
