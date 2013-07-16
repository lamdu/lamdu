{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( Infer, Context, Error(..)
  , infer, unify
  , emptyContext
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (mempty)
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
  }

rename :: Map Guid Guid -> Guid -> Guid
rename renames guid = fromMaybe guid $ renames ^. Lens.at guid

renameRefVars :: Map Guid Guid -> RefVars -> RefVars
renameRefVars renames (RefVars scope getVars) =
  RefVars (Map.mapKeys mapping scope) (Set.map mapping getVars)
  where
    mapping = rename renames

data Error def = VarEscapesScope | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)

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

unify :: Eq def => Ref -> Ref -> Infer def Ref
unify = unifyRename Map.empty

infer ::
  Map Guid Ref -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope = exprIntoSTV scope . ExprUtil.annotateUsedVars

-- With hole apply vals and hole types
exprIntoSTV ::
  Map Guid Ref ->
  Expr.Expression (LoadedDef def) (Set Guid, a) ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body (usedVars, pl)) = do
  (newBody, typeRef) <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      let
        newScope =
          scope
          & Lens.at paramGuid .~
            Just (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      typeRef <- mkHoleRef
      return
        ( Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
        , typeRef
        )
    Expr.BodyLeaf leaf@(Expr.GetVariable (Expr.ParameterRef guid)) ->
      return
      ( Expr.BodyLeaf leaf
      , unsafeUnjust "GetVar out of scope!" $ scope ^. Lens.at guid
      )
    Expr.BodyLeaf leaf@(Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) ->
      return (Expr.BodyLeaf leaf, ref)
    _ ->
      (,)
      <$> (body & Lens.traverse %%~ exprIntoSTV scope)
      <*> mkHoleRef
  valRef <-
    newBody
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & RefData (RefVars scope usedVars)
    & circumcizeApply
    & ExprRefs.fresh
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    mkHoleRef = ExprRefs.fresh . RefData (RefVars scope Set.empty) $ ExprLens.bodyHole # ()
    circumcizeApply (RefData (RefVars s _) Expr.BodyApply{}) =
      RefData (RefVars s Set.empty) $ ExprLens.bodyHole # ()
    circumcizeApply x = x
