{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Lamdu.Sugar.Convert.Composite
    ( convertEmpty, BodyPrism, convert
    , ExtendVal(..), extendTag, extendValI, extendRest
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValBody, ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

deleteItem ::
    Monad m =>
    HRef m # V.Term -> ValI m ->
    ConvertM m (T m EntityId)
deleteItem stored restI =
    ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> Lens.mapped %~ EntityId.ofValI

convertAddItem ::
    Monad m =>
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Set T.Tag ->
    Input.Payload m a # V.Term ->
    ConvertM m (TagReplace InternalName (T m) (T m) EntityId)
convertAddItem extendOp existingTags pl =
    do
        addItem <-
            ConvertM.typeProtectedSetToVal
            <&>
            \protectedSetToVal tag ->
            do
                DataOps.CompositeExtendResult newValI resultI <- extendOp tag (stored ^. ExprIRef.iref)
                _ <- protectedSetToVal stored resultI
                DataOps.setTagOrder tag (Set.size existingTags)
                EntityId.ofValI newValI & pure
        ConvertTag.replace nameWithoutContext existingTags ConvertTag.RequireTag
            (EntityId.ofTag (pl ^. Input.entityId)) addItem
    where
        stored = pl ^. Input.stored

data ExtendVal m rest = ExtendVal
    { _extendTag :: T.Tag
    , _extendValI :: ValI m
    , _extendRest :: rest
    }
Lens.makeLenses ''ExtendVal

convertExtend ::
    Monad m =>
    (T.Tag -> ValI m -> ValI m -> ExprIRef.ValBody m) ->
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Annotated b # Term EvalPrep InternalName (T m) (T m) ->
    Input.Payload m a # V.Term ->
    ExtendVal m (Input.Payload m a # V.Term) ->
    Composite EvalPrep InternalName (T m) (T m) # Annotated b ->
    ConvertM m (Composite EvalPrep InternalName (T m) (T m) # Annotated b)
convertExtend cons extendOp valS exprPl extendV restC =
    do
        itemS <-
            convertItem cons (exprPl ^. Input.stored)
            (extendV ^. extendRest . Input.entityId) (Set.fromList restTags) valS
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref))
        punSugar <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.fieldPuns)
        let addItem =
                do
                    guard punSugar
                    getVar <- itemS ^? ciExpr . hVal . _BodyGetVar
                    name <- getVar ^? SugarLens.getVarName
                    _ <- internalNameMatch (itemS ^. ciTag . tagRefTag . tagName) name
                    let punned = Ann (Const (itemS ^. ciExpr . annotation)) (Const getVar)
                    Just (cPunnedItems %~ (punned :))
                & fromMaybe (cItems %~ (itemS :))
        addItemAction <- convertAddItem extendOp (Set.fromList (extendV ^. extendTag : restTags)) exprPl
        addItem restC
            & cAddItem .~ addItemAction
            & pure
    where
        restTags = restC ^.. cItems . traverse . ciTag . tagRefTag . tagVal

convertOneItemOpenComposite ::
    Monad m =>
    V.Leaf ->
    (T.Tag -> ValI m -> ValI m -> ExprIRef.ValBody m) ->
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    k # Term EvalPrep InternalName (T m) (T m) ->
    k # Term EvalPrep InternalName (T m) (T m) ->
    Input.Payload m a # V.Term ->
    ExtendVal m (Input.Payload m a # V.Term) ->
    ConvertM m (Composite EvalPrep InternalName (T m) (T m) # k)
convertOneItemOpenComposite leaf cons extendOp valS restS exprPl extendV =
    Composite
    <$> ( convertItem cons
            (exprPl ^. Input.stored) (extendV ^. extendRest . Input.entityId) mempty valS
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref))
            <&> (:[])
        )
    <*> pure []
    <*> (convertOpenCompositeActions leaf (extendV ^. extendRest . Input.stored) <&> (`OpenComposite` restS))
    <*> convertAddItem extendOp (Set.singleton (extendV ^. extendTag)) exprPl

convertOpenCompositeActions ::
    Monad m =>
    V.Leaf -> HRef m # V.Term -> ConvertM m (OpenCompositeActions (T m))
convertOpenCompositeActions leaf stored =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal ->
    OpenCompositeActions
    { _openCompositeClose =
        ExprIRef.newValI (V.BLeaf leaf)
        >>= protectedSetToVal stored
        <&> EntityId.ofValI
    }

convertEmpty ::
    Monad m =>
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Input.Payload m a # V.Term ->
    ConvertM m (Composite v InternalName (T m) (T m) expr)
convertEmpty extendOp exprPl =
    do
        actions <-
            ConvertM.postProcessAssert
            <&>
            \postProcess ->
            ClosedCompositeActions
            { _closedCompositeOpen =
                DataOps.replaceWithHole (exprPl ^. Input.stored)
                <* postProcess
                <&> EntityId.ofValI
            }
        addItem <- convertAddItem extendOp mempty exprPl
        pure Composite
            { _cItems = []
            , _cPunnedItems = []
            , _cTail = ClosedComposite actions
            , _cAddItem = addItem
            }

convertItem ::
    Monad m =>
    (T.Tag -> ValI m -> ValI m -> ExprIRef.ValBody m) ->
    HRef m # V.Term ->
    EntityId -> Set T.Tag ->
    h # Term EvalPrep InternalName (T m) (T m) ->
    -- Using tuple in place of shared RecExtend/Case structure (no such in lamdu-calculus)
    ExtendVal m (ValI m) ->
    ConvertM m (CompositeItem EvalPrep InternalName (T m) (T m) # h)
convertItem cons stored inst forbiddenTags exprS extendVal =
    do
        delItem <- deleteItem stored restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons newTag exprI restI & ExprIRef.writeValI valI
                    protectedSetToVal stored valI & void
                where
                    valI = stored ^. ExprIRef.iref
        tagS <- ConvertTag.ref tag nameWithoutContext forbiddenTags (EntityId.ofTag inst) setTag
        pure CompositeItem
            { _ciTag = tagS
            , _ciExpr = exprS
            , _ciDelete = delItem
            }
    where
        ExtendVal tag exprI restI = extendVal

type BodyPrism m a =
    Lens.Prism'
    (Term EvalPrep InternalName (T m) (T m) # Annotated (ConvertPayload m a))
    (Composite EvalPrep InternalName (T m) (T m) # Annotated (ConvertPayload m a))

convert ::
    (Monad m, Monoid a) =>
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    V.Leaf ->
    (T.Tag -> ValI m -> ValI m -> ValBody m) -> BodyPrism m a ->
    ExpressionU EvalPrep m a ->
    ExpressionU EvalPrep m a -> Input.Payload m a # V.Term ->
    ExtendVal m (Input.Payload m a # V.Term) ->
    ConvertM m (ExpressionU EvalPrep m a)
convert op empty cons prism valS restS exprPl extendV =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.composite) >>=
    \case
    False -> convertOneItem
    True ->
        case restS ^? hVal . prism of
        Nothing -> convertOneItem
        Just r ->
            convertExtend cons op valS exprPl extendV r
            <&> (prism #)
            -- Closed sugar Composites use their tail as an entity id,
            -- unlike other sugar constructs.  All the extend entity ids
            -- are "hidden", the vals are directly sugared separately, so
            -- using addActions to add the hidden payloads is complex. No
            -- subexprs given will add no hidden payloads. Then we add the
            -- extend only to pUserData as the hidden payload
            >>= addActions (Const ()) exprPl
            <&> annotation . pInput . Input.entityId .~ restS ^. annotation . pInput . Input.entityId
            <&> annotation . pInput . Input.userData <>~
                (exprPl ^. Input.userData <> restS ^. annotation . pInput . Input.userData)
    where
        convertOneItem =
            convertOneItemOpenComposite empty cons op valS restS exprPl extendV
            <&> (prism #)
            >>= addActions (Const ()) exprPl
