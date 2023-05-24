{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Convert.Composite
    ( convertEmpty, convert, CompositeType(..)
    , ExtendVal(..), extendTag, extendValI, extendRest
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Set as Set
import           Hyper.Type.Functor (F(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.IRef (unsafeFromUUID)
import           Revision.Deltum.Transaction (Transaction, newKey)

import           Lamdu.Prelude

data CompositeType = CompRecord | CompCase

data ExtendVal m rest = ExtendVal
    { _extendTag :: T.Tag
    , _extendValI :: ValI m
    , _extendRest :: rest
    }
Lens.makeLenses ''ExtendVal

type T = Transaction

bodyPrism ::
    (Lens.Choice p, Applicative f) =>
    CompositeType ->
    Lens.Optic' p f
    (Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
    (Composite v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
bodyPrism CompRecord = _BodyRecord
bodyPrism CompCase = _BodyPostfixFunc . _PfCase

closed :: CompositeType -> V.Leaf
closed =
    \case
    CompRecord -> V.LRecEmpty
    CompCase -> V.LAbsurd

cons :: CompositeType -> V.RowExtend T.Tag V.Term V.Term # h -> V.Term # h
cons =
    \case
    CompRecord -> V.BRecExtend
    CompCase -> V.BCase

deleteItem ::
    Monad m =>
    HRef m # V.Term -> ValI m ->
    ConvertM m (T m ())
deleteItem stored restI = ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> void

convertAddItem ::
    Monad m =>
    CompositeType ->
    Set T.Tag ->
    HRef m # V.Term ->
    ConvertM m (OnceT (T m) (TagChoice InternalName (T m)))
convertAddItem compType existingTags stored =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        genNewExtendId <- transaction newKey & ConvertM.convertOnce
        let resultInfo dstKey =
                ConvertTag.TagResultInfo <$> EntityId.ofTag (EntityId.ofIRef dst) <*> addItem
                where
                    dst = unsafeFromUUID dstKey
                    addItem tag =
                        do
                            DataOps.newHole
                                >>= ExprIRef.writeValI (F dst) . cons compType . (V.RowExtend tag ?? stored ^. ExprIRef.iref)
                            _ <- protectedSetToVal stored (F dst)
                            DataOps.setTagOrder tag (Set.size existingTags)
        ConvertTag.replace nameWithoutContext existingTags genNewExtendId resultInfo >>= ConvertM . lift

convertExtend ::
    Monad m =>
    CompositeType ->
    Annotated b # Term v InternalName (OnceT (T m)) (T m) ->
    Input.Payload m # V.Term ->
    ExtendVal m (Input.Payload m # V.Term) ->
    Composite v InternalName (OnceT (T m)) (T m) # Annotated b ->
    ConvertM m (Composite v InternalName (OnceT (T m)) (T m) # Annotated b)
convertExtend compType valS exprPl extendV restC =
    do
        addItemAction <- convertAddItem compType (Set.fromList (extendV ^. extendTag : restTags)) (exprPl ^. Input.stored)
        itemS <-
            convertItem addItemAction compType exprPl
            (Set.fromList restTags) valS
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref))
        punSugar <- Lens.view (ConvertM.scSugars . Config.fieldPuns)
        let addItem items =
                Just TaggedListBody
                { _tlHead = itemS
                , _tlTail =
                    items ^.. Lens._Just . SugarLens.taggedListBodyItems
                    <&> tiAddAfter .~ addItemAction
                    <&> (`TaggedSwappableItem` pure ())
                }
        do
            guard punSugar
            getVar <- itemS ^? tiValue . hVal . _BodyLeaf . _LeafGetVar
            _ <- internalNameMatch (itemS ^. tiTag . tagRefTag . tagName) (getVar ^. vName)
            let punned =
                    PunnedVar
                    { _pvVar = Ann (Const (itemS ^. tiValue . annotation)) (Const getVar)
                    , _pvTagEntityId = itemS ^. tiTag . tagRefTag . tagInstance
                    }
            Just (restC & cPunnedItems %~ (punned :))
            & fromMaybe (restC & cList . tlItems %~ addItem)
            & cList . tlAddFirst .~ addItemAction
            & pure
    where
        restTags =
            restC ^..
            cList . tlItems . Lens._Just . (tlHead <> tlTail . traverse . tsiItem) .
            tiTag . tagRefTag . tagVal

convertOneItemOpenComposite ::
    Monad m =>
    CompositeType ->
    ExpressionU v m ->
    ExpressionU v m ->
    Input.Payload m # V.Term ->
    ExtendVal m (Input.Payload m # V.Term) ->
    ConvertM m (Composite v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
convertOneItemOpenComposite compType valS restS exprPl extendV =
    do
        addItem <- convertAddItem compType (Set.singleton (extendV ^. extendTag)) (exprPl ^. Input.stored)
        item <-
            convertItem addItem compType exprPl mempty valS
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref))
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let close =
                closed compType & V.BLeaf & ExprIRef.newValI >>=
                protectedSetToVal (restS ^. annotation . pUnsugared . hAnn . Input.stored)
                <&> EntityId.ofValI
        pure Composite
            { _cList = TaggedList
                { _tlAddFirst = addItem
                , _tlItems = Just (TaggedListBody item [])
                }
            , _cPunnedItems = []
            , _cTail =
                restS
                & annotation . pActions . delete .~ Delete close
                & OpenCompositeTail
            }

convertEmpty ::
    Monad m =>
    CompositeType -> HRef m # V.Term ->
    ConvertM m (Composite v InternalName (OnceT (T m)) (T m) expr)
convertEmpty compType stored =
    do
        actions <-
            ConvertM.postProcessAssert
            <&>
            \postProcess ->
            ClosedCompositeActions
            { _closedCompositeOpen =
                DataOps.replaceWithHole stored
                <* postProcess
                <&> EntityId.ofValI
            }
        addItem <- convertAddItem compType mempty stored
        pure Composite
            { _cList = TaggedList
                { _tlAddFirst = addItem
                , _tlItems = Nothing
                }
            , _cPunnedItems = []
            , _cTail = ClosedCompositeTail actions
            }

convertItem ::
    Monad m =>
    OnceT (T m) (TagChoice InternalName (T m)) ->
    CompositeType ->
    Input.Payload m # V.Term ->
    Set T.Tag ->
    h # Term v InternalName (OnceT (T m)) (T m) ->
    -- Using tuple in place of shared RecExtend/Case structure (no such in lamdu-calculus)
    ExtendVal m (ValI m) ->
    ConvertM m (TaggedItem InternalName (OnceT (T m)) (T m) (h # Term v InternalName (OnceT (T m)) (T m)))
convertItem addItem compType exprPl forbiddenTags exprS extendVal =
    do
        delItem <- deleteItem (exprPl ^. Input.stored) restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    V.RowExtend newTag exprI restI & cons compType & ExprIRef.writeValI valI
                    protectedSetToVal (exprPl ^. Input.stored) valI & void
                where
                    valI = exprPl ^. Input.stored . ExprIRef.iref
        let resultInfo () = ConvertTag.TagResultInfo <$> EntityId.ofTag (exprPl ^. Input.entityId) <*> setTag
        tagS <- ConvertTag.ref tag Nothing forbiddenTags (pure ()) resultInfo >>= ConvertM . lift
        pure TaggedItem
            { _tiTag = tagS
            , _tiValue = exprS
            , _tiAddAfter = addItem
            , _tiDelete = delItem
            }
    where
        ExtendVal tag exprI restI = extendVal

convert ::
    Monad m =>
    CompositeType ->
    ExpressionU v m ->
    ExpressionU v m -> Ann (Input.Payload m) # V.Term ->
    ExtendVal m (Input.Payload m # V.Term) ->
    ConvertM m (ExpressionU v m)
convert compType valS restS expr extendV =
    Lens.view (ConvertM.scSugars . Config.composite) >>=
    \case
    False -> convertOneItem
    True ->
        case restS ^? hVal . bodyPrism compType of
        Nothing -> convertOneItem
        Just r ->
            convertExtend compType valS (expr ^. hAnn) extendV r
            <&> (bodyPrism compType #)
            >>= addActions expr
            -- Closed sugar Composites use their tail as an entity id.
            <&> annotation . pEntityId .~ restS ^. annotation . pEntityId
    where
        convertOneItem =
            convertOneItemOpenComposite compType valS restS (expr ^. hAnn) extendV
            <&> (bodyPrism compType #)
            >>= addActions expr
