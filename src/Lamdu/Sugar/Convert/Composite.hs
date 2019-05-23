{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Lamdu.Sugar.Convert.Composite
    ( convertEmpty, BodyPrism, convert
    , ExtendVal(..), extendTag, extendValI, extendRest
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann(..), ann, val)
import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValBody, ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTagRef, convertTagReplace, AllowAnonTag(..))
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

deleteItem ::
    Monad m =>
    ValP m -> ValI m ->
    ConvertM m (T m EntityId)
deleteItem stored restI =
    ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> Lens.mapped %~ EntityId.ofValI

convertAddItem ::
    Monad m =>
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Set T.Tag ->
    Input.Payload m a ->
    ConvertM m (TagReplace InternalName (T m) (T m) EntityId)
convertAddItem extendOp existingTags pl =
    do
        addItem <-
            ConvertM.typeProtectedSetToVal
            <&>
            \protectedSetToVal tag ->
            do
                DataOps.CompositeExtendResult newValI resultI <- extendOp tag (stored ^. Property.pVal)
                _ <- protectedSetToVal stored resultI
                DataOps.setTagOrder tag (Set.size existingTags)
                EntityId.ofValI newValI & pure
        convertTagReplace nameWithoutContext existingTags RequireTag (EntityId.ofTag (pl ^. Input.entityId)) addItem
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
    expr ->
    Input.Payload m a ->
    ExtendVal m (Input.Payload m a) ->
    Composite InternalName (T m) (T m) expr ->
    ConvertM m (Composite InternalName (T m) (T m) expr)
convertExtend cons extendOp valS exprPl extendV restC =
    do
        itemS <-
            convertItem cons (exprPl ^. Input.stored)
            (extendV ^. extendRest . Input.entityId) (Set.fromList restTags) valS
            (extendV & extendRest %~ (^. Input.stored . Property.pVal))
        addItem <- convertAddItem extendOp (Set.fromList (extendV ^. extendTag : restTags)) exprPl
        restC
            & cAddItem .~ addItem
            & cItems %~ (itemS :)
            & pure
    where
        restTags = restC ^.. cItems . traverse . ciTag . tagRefTag . tagVal

convertOneItemOpenComposite ::
    Monad m =>
    V.Leaf ->
    (T.Tag -> ValI m -> ValI m -> ExprIRef.ValBody m) ->
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    expr -> expr ->
    Input.Payload m a ->
    ExtendVal m (Input.Payload m a) ->
    ConvertM m (Composite InternalName (T m) (T m) expr)
convertOneItemOpenComposite leaf cons extendOp valS restS exprPl extendV =
    Composite
    <$> ( convertItem cons
            (exprPl ^. Input.stored) (extendV ^. extendRest . Input.entityId) mempty valS
            (extendV & extendRest %~ (^. Input.stored . Property.pVal))
            <&> (:[])
        )
    <*> (convertOpenCompositeActions leaf (extendV ^. extendRest . Input.stored) <&> (`OpenComposite` restS))
    <*> convertAddItem extendOp (Set.singleton (extendV ^. extendTag)) exprPl

convertOpenCompositeActions ::
    Monad m => V.Leaf -> ValP m -> ConvertM m (OpenCompositeActions (T m))
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
    Input.Payload m a ->
    ConvertM m (Composite InternalName (T m) (T m) expr)
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
            , _cTail = ClosedComposite actions
            , _cAddItem = addItem
            }

convertItem ::
    Monad m =>
    (T.Tag -> ValI m -> ValI m -> ExprIRef.ValBody m) ->
    ValP m ->
    EntityId -> Set T.Tag -> expr ->
    -- Using tuple in place of shared RecExtend/Case structure (no such in lamdu-calculus)
    ExtendVal m (ValI m) ->
    ConvertM m (CompositeItem InternalName (T m) (T m) expr)
convertItem cons stored inst forbiddenTags exprS extendVal =
    do
        delItem <- deleteItem stored restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons newTag exprI restI & ExprIRef.writeValI valI
                    protectedSetToVal stored valI & void
                where
                    valI = stored ^. Property.pVal
        tagS <- convertTagRef tag nameWithoutContext forbiddenTags (EntityId.ofTag inst) setTag
        pure CompositeItem
            { _ciTag = tagS
            , _ciExpr = exprS
            , _ciDelete = delItem
            }
    where
        ExtendVal tag exprI restI = extendVal

type BodyPrism m a =
    Lens.Prism'
    (Tree (Body InternalName (T m) (T m)) (Ann (ConvertPayload m a)))
    (Composite InternalName (T m) (T m) (ExpressionU m a))

convert ::
    (Monad m, Monoid a) =>
    (T.Tag -> ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    V.Leaf ->
    (T.Tag -> ValI m -> ValI m -> ValBody m) -> BodyPrism m a ->
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    ExtendVal m (Input.Payload m a) ->
    ConvertM m (ExpressionU m a)
convert op empty cons prism valS restS exprPl extendV =
    case restS ^? val . prism of
    Just r ->
        convertExtend cons op valS exprPl extendV r
        <&> (prism #)
        -- Closed sugar Composites use their tail as an entity id,
        -- unlike other sugar constructs.  All the extend entity ids
        -- are "hidden", the vals are directly sugared separately, so
        -- using addActions to add the hidden payloads is complex. No
        -- subexprs given will add no hidden payloads. Then we add the
        -- extend only to pUserData as the hidden payload
        >>= addActions [] exprPl
        <&> ann . pInput . Input.entityId .~ restS ^. ann . pInput . Input.entityId
        <&> ann . pInput . Input.userData <>~
            (exprPl ^. Input.userData <> restS ^. ann . pInput . Input.userData)
    Nothing ->
        convertOneItemOpenComposite empty cons op valS restS exprPl extendV
        <&> (prism #)
        >>= addActions [] exprPl
