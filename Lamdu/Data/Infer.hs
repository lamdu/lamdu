{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify
  , emptyContext
  -- Re-export:
  , Context
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

emptyContext :: Context def
emptyContext =
  Context
  { _ctxExprRefs =
    ExprRefs
    { _exprRefsUF = UF.empty
    , _exprRefsData = mempty
    }
  , _ctxDefRefs = Map.empty
  }

rename :: Map Guid Guid -> Guid -> Guid
rename renames guid = fromMaybe guid $ renames ^. Lens.at guid

renameRefVars :: Map Guid Guid -> RefVars -> RefVars
renameRefVars renames (RefVars scope getVars) =
  RefVars (Map.mapKeys mapping scope) (Set.map mapping getVars)
  where
    mapping = rename renames

data Error def
  = VarEscapesScope
  | VarNotInScope
  | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)
  deriving (Show)

type Infer def = StateT (Context def) (Either (Error def))

mergeVars ::
  Map Guid Guid -> RefVars -> RefVars ->
  Infer def RefVars
mergeVars renames aRefVars bRefVars
  | any (`Map.notMember` aScope) (Set.toList bGetVars)
  || any (`Map.notMember` bScope) (Set.toList aGetVars)
  = lift $ Left VarEscapesScope
  | otherwise
  = (`RefVars` Set.union aGetVars bGetVars)
    <$> sequenceA (Map.intersectionWith verifyEquiv aScope bScope)
  where
    RefVars aScope aGetVars = aRefVars
    RefVars bScope bGetVars = renameRefVars renames bRefVars
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- ExprRefs.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

mergeBodies ::
  Eq def =>
  Map Guid Guid ->
  Expr.Body def Ref ->
  Expr.Body def Ref ->
  Infer def (Expr.Body def Ref)
mergeBodies _ a (Expr.BodyLeaf Expr.Hole) = return a
mergeBodies renames (Expr.BodyLeaf Expr.Hole) b =
  b & ExprLens.bodyParameterRef %~ rename renames & return
mergeBodies renames a b = do
  case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther matchGetPar a b of
    Nothing -> lift . Left $ Mismatch a b
    Just mkBody -> mkBody
  where
    matchLamResult aGuid bGuid aRef bRef = do
      unifyRename (renames & Lens.at bGuid .~ Just aGuid) aRef bRef
    matchOther = unifyRename renames
    matchGetPar aGuid bGuid = aGuid == rename renames bGuid

mergeRefData :: Eq def => Map Guid Guid -> RefData def -> RefData def -> Infer def (RefData def)
mergeRefData renames (RefData aVars aBody) (RefData bVars bBody) =
  RefData
  <$> mergeVars renames aVars bVars
  <*> mergeBodies renames aBody bBody

unifyRename :: Eq def => Map Guid Guid -> Ref -> Ref -> Infer def Ref
unifyRename = ExprRefs.unifyRefs . mergeRefData

unify :: Eq def => Ref -> Ref -> Infer def ()
unify x y = void $ unifyRename Map.empty x y

infer ::
  Map Guid Ref -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope = exprIntoSTV scope . ExprUtil.annotateUsedVars

-- TODO: move to appropriate module
emptyRefVars :: RefVars
emptyRefVars = RefVars Map.empty Set.empty

makeHoleRef :: MonadA m => Map Guid Ref -> StateT (Context def) m Ref
makeHoleRef scope =
  ExprRefs.fresh . RefData (RefVars scope Set.empty) $ ExprLens.bodyHole # ()

makeTypeRef ::
  Map Guid Ref ->
  Expr.Body (LoadedDef def) (Expr.Expression (LoadedDef def) (ScopedTypedValue, a)) ->
  Infer def Ref
makeTypeRef scope (Expr.BodyLeaf leaf) =
  case leaf of
  Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref)) -> pure ref
  Expr.GetVariable (Expr.ParameterRef guid) ->
    case scope ^. Lens.at guid of
    Nothing -> lift $ Left VarNotInScope
    Just ref -> pure ref
  Expr.LiteralInteger _ -> resNoScope $ ExprLens.bodyIntegerType # ()
  Expr.Type -> resType
  Expr.IntegerType -> resType
  Expr.Hole -> makeHoleRef scope
  Expr.TagType -> resType
  Expr.Tag _ -> resNoScope $ ExprLens.bodyTagType # ()
  where
    resType = resNoScope $ ExprLens.bodyType # ()
    resNoScope = ExprRefs.fresh . RefData emptyRefVars
makeTypeRef _ (Expr.BodyLam (Expr.Lam Expr.KType _ _ _)) =
  ExprRefs.fresh . RefData emptyRefVars $ ExprLens.bodyType # ()
makeTypeRef scope (Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType body)) =
  ExprRefs.fresh . RefData (RefVars scope Set.empty) . Expr.BodyLam .
  Expr.Lam Expr.KType paramGuid (paramType ^. Expr.ePayload . Lens._1 . stvTV . tvVal) =<<
  makeHoleRef (body ^. Expr.ePayload . Lens._1 . stvScope)
makeTypeRef _ (Expr.BodyRecord (Expr.Record Expr.KType _)) =
  ExprRefs.fresh . RefData emptyRefVars $ ExprLens.bodyType # ()
makeTypeRef scope (Expr.BodyRecord (Expr.Record Expr.KVal fields)) =
  fields
  & Lens.traverse %%~ onField
  >>= ExprRefs.fresh . RefData (RefVars scope Set.empty) .
      Expr.BodyRecord . Expr.Record Expr.KType
  where
    onField (tag, _) =
      (,) (tag ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      <$> makeHoleRef scope
makeTypeRef scope (Expr.BodyApply _) = makeHoleRef scope
makeTypeRef scope (Expr.BodyGetField _) = makeHoleRef scope

-- With hole apply vals and hole types
exprIntoSTV ::
  Map Guid Ref ->
  Expr.Expression (LoadedDef def) (Set Guid, a) ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body (usedVars, pl)) = do
  newBody <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      let
        newScope =
          scope
          & Lens.at paramGuid .~
            Just (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      pure . Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
    _ ->
      body & Lens.traverse %%~ exprIntoSTV scope
  valRef <-
    newBody
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & RefData (RefVars scope usedVars)
    & circumcizeApply
    & ExprRefs.fresh
  typeRef <- makeTypeRef scope newBody
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    circumcizeApply (RefData (RefVars s _) Expr.BodyApply{}) =
      RefData (RefVars s Set.empty) $ ExprLens.bodyHole # ()
    circumcizeApply x = x
