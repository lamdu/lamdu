{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Internal
    ( ConvertPayload(..), pInput, pActions
    , EvalPrep(..), eType, eEvalId, eLambdas
    , InternalName(..), inTag, inContext
    , internalNameMatch
    , nameWithoutContext, nameWithContext, taggedName
    , ExpressionU
    , replaceWith
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction, getP)
import           Data.UUID.Types (UUID)
import           Hyper
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Expr.IRef (HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ConvertPayload m a = ConvertPayload
    { -- Stored of top-level subtree for sugar expression subtree
      _pInput :: Input.Payload m a # V.Term
    , _pActions :: NodeActions InternalName (OnceT (T m)) (T m)
    }

data EvalPrep = EvalPrep
    { _eType :: Pure # T.Type
    , _eEvalId :: UUID
    , -- Identifiers of lambdas that were "swallowed by the sugar".
      -- This happens in let-items (redexes) and in else-if clauses.
      -- Their evaluation scopes are translated to the parent scope
      -- which is exposed by the sugar.
      _eLambdas :: [UUID]
    }

-- | Tags have internal names.
--
-- Some entities (e.g: record fields) directly contain tags, so their
-- InternalName has a Nothing as the inContext
--
-- Other entities (e.g: Nominals[TId] or Definitions/Parameters[Var])
-- are associated with tags, so their InternalName has the UUID of the
-- Nominal/Def/Parameter as context
data InternalName = InternalName
    { _inContext :: Maybe UUID
    , _inTag :: T.Tag
    } deriving (Eq, Ord, Show)

-- 2 Internal names clash if their UUIDs mismatch OR if they
-- positively have Vars that mismatch
--
-- i.e: Having no Var (e.g: a record field) means it matches the exact
-- same tag set for a Var
internalNameMatch :: InternalName -> InternalName -> Maybe InternalName
internalNameMatch a@(InternalName aMVar aUuid) b@(InternalName bMVar bUuid)
    | aUuid /= bUuid = Nothing
    | otherwise =
        case (aMVar, bMVar) of
        (Just aVar, Just bVar)
            | aVar == bVar -> Just a
            | otherwise -> Nothing
        (Nothing, _) -> Just b
        (_, Nothing) -> Just a

nameWithoutContext :: T.Tag -> InternalName
nameWithoutContext tag =
    InternalName
    { _inContext = Nothing
    , _inTag = tag
    }

nameWithContext :: UniqueId.ToUUID a => a -> T.Tag -> InternalName
nameWithContext param tag =
    InternalName
    { _inContext = Just (UniqueId.toUUID param)
    , _inTag = tag
    }

taggedName :: (MonadTransaction n m, UniqueId.ToUUID a) => a -> m InternalName
taggedName x = Anchors.assocTag x & getP <&> nameWithContext x

type ExpressionU v m a = Annotated (ConvertPayload m a) # Term v InternalName (OnceT (T m)) (T m)

replaceWith ::
    Monad m =>
    HRef m # V.Term -> HRef m # V.Term ->
    T m EntityId
replaceWith parentP replacerP =
    EntityId.ofValI replacerI <$ (parentP ^. ExprIRef.setIref) replacerI
    where
        replacerI = replacerP ^. ExprIRef.iref

Lens.makeLenses ''ConvertPayload
Lens.makeLenses ''EvalPrep
Lens.makeLenses ''InternalName
