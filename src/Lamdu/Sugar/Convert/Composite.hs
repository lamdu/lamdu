{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Convert.Composite
    ( convertEmpty, convert, CompositeType(..)
    , ExtendVal(..), extendTag, extendValI, extendRest
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Set as Set
import           Hyper
import           Hyper.Type.Functor (F(..), _F)
import           Hyper.Syntax.Row (eRest)
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
import           Revision.Deltum.IRef (IRef(..), unsafeFromUUID)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

data CompositeType = CompRecord | CompCase

data ExtendVal m rest = ExtendVal
    { _extendTag :: T.Tag
    , _extendValI :: ValI m
    , _extendRest :: rest
    }
Lens.makeLenses ''ExtendVal

type T = Transaction

sugarCons ::
    (Lens.Choice p, Applicative f) =>
    CompositeType -> Lens.Optic' p f (Term v name i o # h) (Composite v name i o # h)
sugarCons CompRecord = _BodyRecord
sugarCons CompCase = _BodyPostfixFunc . _PfCase

closed :: CompositeType -> V.Leaf
closed =
    \case
    CompRecord -> V.LRecEmpty
    CompCase -> V.LAbsurd

cons ::
    (Lens.Choice p, Applicative f) =>
    CompositeType -> Lens.Optic' p f (V.Term # h) (V.RowExtend T.Tag V.Term V.Term # h)
cons =
    \case
    CompRecord -> V._BRecExtend
    CompCase -> V._BCase

data IsLastField = LastField | MoreFields

deleteItem ::
    Monad m =>
    HRef m # V.Term -> ValI m -> CompositeType -> IsLastField ->
    ConvertM m (T m ())
deleteItem stored restI _ MoreFields =
    ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> void
deleteItem stored _ compType LastField =
    -- For the last field we set existing ref to empty record to preserve its id
    ConvertM.typeProtectedSetToVal <&>
    \setToVal ->
    do
        closed compType & V.BLeaf & ExprIRef.writeValI (stored ^. ExprIRef.iref)
        setToVal stored (stored ^. ExprIRef.iref) & void

convertAddItem ::
    Monad m =>
    CompositeType ->
    Set T.Tag ->
    HRef m # V.Term ->
    ConvertM m (OnceT (T m) (TagChoice InternalName (T m)))
convertAddItem compType existingTags stored =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        genNewExtendId <-
            stored ^. ExprIRef.iref & ExprIRef.readRecursively
            <&> uuid . (^. _F) . newTagId compType
            & transaction & ConvertM.convertOnce
        let resultInfo dstKey =
                ConvertTag.TagResultInfo <$> EntityId.ofTag (EntityId.ofIRef dst) <*> addItem
                where
                    dst = unsafeFromUUID dstKey
                    addItem tag =
                        do
                            stored ^. ExprIRef.iref & ExprIRef.readRecursively
                                <&> addTag tag compType <&> hflipped %~ hmap (const (:*: Const ()))
                                >>= ExprIRef.writeRecursively <&> (^. hAnn . Lens._1)
                                >>= protectedSetToVal stored & void
                            DataOps.setTagOrder tag (Set.size existingTags)
        ConvertTag.replace nameWithoutContext existingTags genNewExtendId resultInfo >>= ConvertM . lift

newTagId :: CompositeType -> Ann a # V.Term -> a # V.Term
newTagId compType (Ann pos body) =
    case body ^? cons compType of
    Just row -> row ^. eRest & newTagId compType
    Nothing -> pos

-- Somewhat hacky implementation of adding the new tag on the inner empty record/case
addTag ::
    T.Tag -> CompositeType ->
    Ann (F (IRef m)) # V.Term -> Ann (ExprIRef.Write m) # V.Term
addTag tag compType term@(Ann pos body) =
    cons compType #
    case body of
    V.BLeaf l | l == closed compType -> V.BLeaf l & Ann ExprIRef.WriteNew & ext
    _ ->
        case body ^? cons compType of
        Just (V.RowExtend t v r) -> addTag tag compType r & V.RowExtend t (existing v)
        Nothing -> existing term & ext
    & Ann (ExprIRef.ExistingRef pos)
    where
        ext = V.RowExtend tag (Ann ExprIRef.WriteNew (V.BLeaf V.LHole))
        existing = hflipped %~ hmap (const ExprIRef.ExistingRef)

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
            (if Lens.has (cList . tlItems . Lens._Nothing) restC then LastField else MoreFields)
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
            (extendV & extendRest %~ (^. Input.stored . ExprIRef.iref)) MoreFields
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
    IsLastField ->
    ConvertM m (TaggedItem InternalName (OnceT (T m)) (T m) (h # Term v InternalName (OnceT (T m)) (T m)))
convertItem addItem compType exprPl forbiddenTags exprS extendVal isLast =
    do
        delItem <- deleteItem (exprPl ^. Input.stored) restI compType isLast
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons compType # V.RowExtend newTag exprI restI & ExprIRef.writeValI valI
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
        case restS ^? hVal . sugarCons compType of
        Nothing -> convertOneItem
        Just r -> convertExtend compType valS (expr ^. hAnn) extendV r
    >>= addActions expr . (sugarCons compType #)
    where
        convertOneItem = convertOneItemOpenComposite compType valS restS (expr ^. hAnn) extendV
