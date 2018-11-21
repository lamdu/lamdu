module Lamdu.Sugar.OrderTags
    ( orderDef, orderType, orderBinder
    , orderedClosedFlatComposite
    ) where

import qualified Control.Lens.Extended as Lens
import           Data.List (sortOn)
import           Data.Tree.Diverse (Node, Ann(..), ann, val)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Tag (tagOrder)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction
type Order m x = x -> T m x

orderByTag :: Monad m => (a -> Sugar.TagInfo name) -> Order m [a]
orderByTag toTag =
    fmap (map fst . sortOn snd) . traverse loadOrder
    where
        loadOrder x =
            toTag x ^. Sugar.tagVal
            & ExprIRef.readTagInfo
            <&> (,) x . (^. tagOrder)

orderComposite :: Monad m => Order m (Sugar.CompositeFields p name (Sugar.Type a))
orderComposite =
    Sugar.compositeFields $
    \fields -> fields & orderByTag (^. _1) >>= traverse . _2 %%~ orderType

orderTBody :: Monad m => Order m (Sugar.TBody name (Sugar.Type name))
orderTBody t =
    t
    & Sugar._TRecord %%~ orderComposite
    >>= Sugar._TVariant %%~ orderComposite
    >>= traverse orderType

orderType :: Monad m => Order m (Sugar.Type name)
orderType = Sugar.tBody %%~ orderTBody

orderRecord ::
    Monad m =>
    Order m (Sugar.Composite name (T m) o (Sugar.Expression name (T m) o (Sugar.Payload name i o a)))
orderRecord r =
    r
    & Sugar.cItems (orderByTag (^. Sugar.ciTag . Sugar.tagInfo))
    >>= traverse orderExpr

orderLabeledApply :: Monad m => Order m (Sugar.LabeledApply name (T m) o (Ann (Sugar.Payload name i o a)))
orderLabeledApply a =
    a
    & Sugar.aAnnotatedArgs %%~ orderByTag (^. Sugar.aaTag)
    >>= SugarLens.labeledApplyChildren pure pure orderExpr

orderCase ::
    Monad m =>
    Order m (Sugar.Case name (T m) o (Sugar.Expression name (T m) o (Sugar.Payload name i o a)))
orderCase = Sugar.cBody %%~ orderRecord

orderLam ::
    Monad m =>
    Order m (Sugar.Lambda name (T m) o (Ann (Sugar.Payload name i o a)))
orderLam = Sugar.lamFunc orderFunction

orderBinderBody ::
    Monad m =>
    Order m (Sugar.Binder name (T m) o (Ann (Sugar.Payload name i o a)))
orderBinderBody (Sugar.BinderExpr x) = orderBody x <&> Sugar.BinderExpr
orderBinderBody (Sugar.BinderLet x) =
    x
    & Sugar.lBody orderBinder
    >>= Sugar.lValue orderAssignment
    <&> Sugar.BinderLet

orderBinder ::
    Monad m =>
    Order m (Node (Ann (Sugar.Payload name i o a)) (Sugar.Binder name (T m) o))
orderBinder = val orderBinderBody

orderElse ::
    Monad m =>
    Order m (Sugar.Else name (T m) o (Ann (Sugar.Payload name i o a)))
orderElse (Sugar.SimpleElse x) = orderBody x <&> Sugar.SimpleElse
orderElse (Sugar.ElseIf x) = Sugar.eiContent orderIfElse x <&> Sugar.ElseIf

orderIfElse ::
    Monad m =>
    Order m (Sugar.IfElse name (T m) o (Ann (Sugar.Payload name i o a)))
orderIfElse x =
    x
    & Sugar.iIf orderExpr
    >>= Sugar.iThen orderExpr
    >>= (Sugar.iElse . val) orderElse

orderBody ::
    Monad m =>
    Order m (Sugar.Body name (T m) o (Ann (Sugar.Payload name i o a)))
orderBody (Sugar.BodyLam l) = orderLam l <&> Sugar.BodyLam
orderBody (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
orderBody (Sugar.BodyLabeledApply a) = orderLabeledApply a <&> Sugar.BodyLabeledApply
orderBody (Sugar.BodyCase c) = orderCase c <&> Sugar.BodyCase
orderBody (Sugar.BodyHole a) = SugarLens.holeTransformExprs orderBinder a & Sugar.BodyHole & pure
orderBody (Sugar.BodyFragment a) =
    a
    & Sugar.fOptions . Lens.mapped . Lens.mapped %~ SugarLens.holeOptionTransformExprs orderBinder
    & Sugar.BodyFragment
    & pure
orderBody (Sugar.BodyIfElse x) = orderIfElse x <&> Sugar.BodyIfElse
orderBody (Sugar.BodyInject x) = (Sugar.iContent . Sugar._InjectVal) orderExpr x <&> Sugar.BodyInject
orderBody (Sugar.BodyToNom x) = traverse orderBinder x <&> Sugar.BodyToNom
orderBody (Sugar.BodyFromNom x) = traverse orderExpr x <&> Sugar.BodyFromNom
orderBody (Sugar.BodySimpleApply x) = traverse orderExpr x <&> Sugar.BodySimpleApply
orderBody (Sugar.BodyGetField x) = traverse orderExpr x <&> Sugar.BodyGetField
orderBody x@Sugar.BodyLiteral{} = pure x
orderBody x@Sugar.BodyGetVar{} = pure x
orderBody x@Sugar.BodyPlaceHolder{} = pure x

orderExpr ::
    Monad m => Order m (Sugar.Expression name (T m) o (Sugar.Payload name i o a))
orderExpr e =
    e
    & ann . Sugar.plAnnotation . SugarLens.annotationTypes %%~ orderType
    >>= val %%~ orderBody
    -- TODO: Skipping ordering of "if"
    >>= val (SugarLens.bodyChildren pure pure pure pure orderBinder orderExpr)

orderFunction ::
    Monad m =>
    Order m (Sugar.Function name (T m) o (Ann (Sugar.Payload name i o a)))
orderFunction =
    -- The ordering for binder params already occurs at the Assignment's conversion,
    -- because it needs to be consistent with the presentation mode.
    Sugar.fBody orderBinder

orderAssignmentBody ::
    Monad m =>
    Order m (Sugar.AssignmentBody name (T m) o (Ann (Sugar.Payload name i o a)))
orderAssignmentBody (Sugar.BodyFunction x) =
    orderFunction x <&> Sugar.BodyFunction
orderAssignmentBody (Sugar.BodyPlain x) =
    Sugar.apBody orderBinderBody x <&> Sugar.BodyPlain

orderAssignment :: Monad m => Order m (Sugar.Assignment name (T m) o (Sugar.Payload name i o a))
orderAssignment = val orderAssignmentBody

orderDef ::
    Monad m => Order m (Sugar.Definition name (T m) o (Sugar.Payload name i o a))
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
