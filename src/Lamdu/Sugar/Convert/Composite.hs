{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Lamdu.Sugar.Convert.Composite
    ( convertEmpty, BodyPrism, convert
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
import           Revision.Deltum.IRef (IRef, unsafeFromUUID)
import           Revision.Deltum.Transaction (Transaction, newKey)

import           Lamdu.Prelude

type T = Transaction

deleteItem ::
    Monad m =>
    HRef m # V.Term -> ValI m ->
    ConvertM m (T m ())
deleteItem stored restI = ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> void

convertAddItem ::
    Monad m =>
    (V.RowExtend T.Tag V.Term V.Term # F (IRef m) -> ExprIRef.ValBody m) ->
    Set T.Tag ->
    HRef m # V.Term ->
    ConvertM m (OnceT (T m) (TagChoice InternalName (T m)))
convertAddItem cons existingTags stored =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        genNewExtendId <- transaction newKey & ConvertM.convertOnce
        let resultInfo dstKey =
                ConvertTag.TagResultInfo <$> EntityId.ofTag (EntityId.ofIRef dst) <*> addItem
                where
                    dst = unsafeFromUUID dstKey
                    addItem tag =
                        do
                            _ <-
                                DataOps.newHole
                                >>= ExprIRef.writeValI (F dst) . cons . (V.RowExtend tag ?? stored ^. ExprIRef.iref)
                            _ <- protectedSetToVal stored (F dst)
                            DataOps.setTagOrder tag (Set.size existingTags)
        ConvertTag.replace nameWithoutContext existingTags genNewExtendId resultInfo >>= ConvertM . lift

data ExtendVal m rest = ExtendVal
    { _extendTag :: T.Tag
    , _extendValI :: ValI m
    , _extendRest :: rest
    }
Lens.makeLenses ''ExtendVal

convertExtend ::
    Monad m =>
    (V.RowExtend T.Tag V.Term V.Term # F (IRef m) -> ExprIRef.ValBody m) ->
    Annotated b # Term v InternalName (OnceT (T m)) (T m) ->
    Input.Payload m # V.Term ->
    ExtendVal m (Input.Payload m # V.Term) ->
    Composite v InternalName (OnceT (T m)) (T m) # Annotated b ->
    ConvertM m (Composite v InternalName (OnceT (T m)) (T m) # Annotated b)
convertExtend cons valS exprPl extendV restC =
    do
        addItemAction <- convertAddItem cons (Set.fromList (extendV ^. extendTag : restTags)) (exprPl ^. Input.stored)
        itemS <-
            convertItem addItemAction cons exprPl
            (Set.fromList restTags) valS
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref))
        punSugar <- Lens.view (ConvertM.scSugars . Config.fieldPuns)
        let addItem items =
                Just TaggedListBody
                { _tlHead = itemS
                , _tlTail = items ^.. Lens._Just . SugarLens.taggedListBodyItems <&> (`TaggedSwappableItem` pure ())
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
    V.Leaf ->
    (V.RowExtend T.Tag V.Term V.Term # F (IRef m) -> ExprIRef.ValBody m) ->
    ExpressionU v m ->
    ExpressionU v m ->
    Input.Payload m # V.Term ->
    ExtendVal m (Input.Payload m # V.Term) ->
    ConvertM m (Composite v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
convertOneItemOpenComposite closed cons valS restS exprPl extendV =
    do
        addItem <- convertAddItem cons (Set.singleton (extendV ^. extendTag)) (exprPl ^. Input.stored)
        item <-
            convertItem addItem cons exprPl mempty valS
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref))
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let close =
                V.BLeaf closed & ExprIRef.newValI >>=
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
    (V.RowExtend T.Tag V.Term V.Term # F (IRef m) -> ExprIRef.ValBody m) ->
    HRef m # V.Term ->
    ConvertM m (Composite v InternalName (OnceT (T m)) (T m) expr)
convertEmpty cons stored =
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
        addItem <- convertAddItem cons mempty stored
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
    (V.RowExtend T.Tag V.Term V.Term # F (IRef m) -> ExprIRef.ValBody m) ->
    Input.Payload m # V.Term ->
    Set T.Tag ->
    h # Term v InternalName (OnceT (T m)) (T m) ->
    -- Using tuple in place of shared RecExtend/Case structure (no such in lamdu-calculus)
    ExtendVal m (ValI m) ->
    ConvertM m (TaggedItem InternalName (OnceT (T m)) (T m) (h # Term v InternalName (OnceT (T m)) (T m)))
convertItem addItem cons exprPl forbiddenTags exprS extendVal =
    do
        delItem <- deleteItem (exprPl ^. Input.stored) restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    V.RowExtend newTag exprI restI & cons & ExprIRef.writeValI valI
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

type BodyPrism m v =
    Lens.Prism'
    (Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
    (Composite v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))

convert ::
    Monad m =>
    V.Leaf ->
    (V.RowExtend T.Tag V.Term V.Term # F (IRef m) -> ExprIRef.ValBody m) ->
    BodyPrism m v ->
    ExpressionU v m ->
    ExpressionU v m -> Ann (Input.Payload m) # V.Term ->
    ExtendVal m (Input.Payload m # V.Term) ->
    ConvertM m (ExpressionU v m)
convert closed cons prism valS restS expr extendV =
    Lens.view (ConvertM.scSugars . Config.composite) >>=
    \case
    False -> convertOneItem
    True ->
        case restS ^? hVal . prism of
        Nothing -> convertOneItem
        Just r ->
            convertExtend cons valS (expr ^. hAnn) extendV r
            <&> (prism #)
            >>= addActions expr
            -- Closed sugar Composites use their tail as an entity id.
            <&> annotation . pEntityId .~ restS ^. annotation . pEntityId
    where
        convertOneItem =
            convertOneItemOpenComposite closed cons valS restS (expr ^. hAnn) extendV
            <&> (prism #)
            >>= addActions expr
