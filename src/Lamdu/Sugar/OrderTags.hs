module Lamdu.Sugar.OrderTags
    ( orderDef, orderType, orderExpr
    , orderedClosedFlatComposite
    ) where

import qualified Control.Lens.Extended as Lens
import           Control.Monad ((>=>))
import           Data.List (sortOn)
import qualified Data.Property as Property
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (assocTagOrder)
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
            & assocTagOrder
            & Property.getP
            <&> (,) x

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

orderRecord :: Monad m => Order m (Sugar.Composite name i o a)
orderRecord = Sugar.cItems %%~ orderByTag (^. Sugar.ciTag . Sugar.tagInfo)

orderLabeledApply :: Monad m => Order m (Sugar.LabeledApply name i o a)
orderLabeledApply = Sugar.aAnnotatedArgs %%~ orderByTag (^. Sugar.aaTag)

orderCase :: Monad m => Order m (Sugar.Case name (T m) o a)
orderCase = Sugar.cBody %%~ orderRecord

orderLam :: Monad m => Order m (Sugar.Lambda name (T m) o (Sugar.Payload name i o a))
orderLam = Sugar.lamFunc orderFunction

orderBody :: Monad m => Order m (Sugar.Body name (T m) o (Sugar.Payload name i o a))
orderBody (Sugar.BodyLam l) = orderLam l <&> Sugar.BodyLam
orderBody (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
orderBody (Sugar.BodyLabeledApply a) = orderLabeledApply a <&> Sugar.BodyLabeledApply
orderBody (Sugar.BodyCase c) = orderCase c <&> Sugar.BodyCase
orderBody (Sugar.BodyHole a) =
    SugarLens.holeTransformExprs (SugarLens.binderContentExprs orderExpr) a
    & Sugar.BodyHole & pure
orderBody (Sugar.BodyFragment a) =
    a
    & Sugar.fOptions . Lens.mapped . Lens.mapped %~
        SugarLens.holeOptionTransformExprs (SugarLens.binderContentExprs orderExpr)
    & Sugar.BodyFragment
    & pure
orderBody x@Sugar.BodyIfElse{} = pure x
orderBody x@Sugar.BodySimpleApply{} = pure x
orderBody x@Sugar.BodyLiteral{} = pure x
orderBody x@Sugar.BodyGetField{} = pure x
orderBody x@Sugar.BodyGetVar{} = pure x
orderBody x@Sugar.BodyInject{} = pure x
orderBody x@Sugar.BodyToNom{} = pure x
orderBody x@Sugar.BodyFromNom{} = pure x
orderBody x@Sugar.BodyPlaceHolder{} = pure x

orderExpr ::
    Monad m => Order m (Sugar.Expression name (T m) o (Sugar.Payload name i o a))
orderExpr e =
    e
    & Sugar.annotation . Sugar.plAnnotation . SugarLens.annotationTypes %%~ orderType
    >>= Sugar.body %%~ orderBody
    >>= Sugar.body . SugarLens.bodyChildren pure pure pure %%~ orderExpr

orderFunction :: Monad m => Order m (Sugar.Function name (T m) o a)
orderFunction =
    -- The ordering for binder params already occurs at the Assignment's conversion,
    -- because it needs to be consistent with the presentation mode.
    pure

orderAssignment :: Monad m => Order m (Sugar.Assignment name (T m) o (Sugar.Payload name i o a))
orderAssignment = (Sugar.aBody . Sugar._BodyFunction . Sugar.afFunction) orderFunction

orderDef ::
    Monad m => Order m (Sugar.Definition name (T m) o (Sugar.Payload name i o a))
orderDef def =
    def
    & SugarLens.defSchemes . Sugar.schemeType %%~ orderType
    >>= Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent
        %%~ (orderAssignment >=> SugarLens.assignmentExprs %%~ orderExpr)

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
