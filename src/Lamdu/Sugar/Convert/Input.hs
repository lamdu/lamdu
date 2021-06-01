-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DataKinds, UndecidableInstances, GADTs, TypeFamilies #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferRes, stored
        , userData, localsInScope, inferScope
        , inferredType, inferredTypeUVar
    , SugarInput, preprocess
    ) where

import           Control.Lens.Extended ((~~>))
import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.AST.FuncType (funcIn)
import           Hyper.Type.Prune (Prune)
import           Hyper.Unify (UVar)
import           Hyper.Unify.Generalize (GTerm(..))
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type, Row)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Expr.IRef (HRef)
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data Payload m a h = Payload
    { _entityId :: EntityId
    , _inferRes :: InferResult (Pure :*: UVar) h
    , _inferScope :: V.Scope # UVar
    , _localsInScope :: [(V.Var, Pure # Type)]
    , _stored :: HRef m h
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    }

Lens.makeLenses ''Payload
makeHTraversableAndBases ''Payload

inferredType :: Lens' (Payload m a # V.Term) (Pure # Type)
inferredType = inferRes . inferResult . _1

inferredTypeUVar :: Lens' (Payload m a # V.Term) (UVar # Type)
inferredTypeUVar = inferRes . inferResult . _2

class SugarInput t where
    prep ::
        V.Scope # UVar -> [(V.Var, Pure # Type)] ->
        Ann (Payload m a) # t ->
        (Map V.Var [EntityId], Ann (Payload m a) # t)
    prep _ _ = (,) mempty

preprocess ::
    SugarInput t =>
    V.Scope # UVar -> [(V.Var, Pure # Type)] ->
    Ann (Payload m a) # t -> Ann (Payload m a) # t
preprocess s l = snd . prep s l

instance SugarInput V.Term where
    prep iScope locals (Ann pl body) =
        case body of
        V.BLam (V.TypedLam var t b) ->
            ( usesOfVar
            , V.TypedLam var t r & V.BLam & Ann (resPl & varRefsOfLambda .~ usesOfVar ^. Lens.ix var)
            )
            where
                (usesOfVar, r) = prep innerScope ((var, varType) : locals) b
                varType = pl ^?! inferredType . _Pure . T._TFun . funcIn
                innerScope =
                    iScope
                    & V.scopeVarTypes . Lens.at var ?~
                        _HFlip # GMono (t ^. hAnn . inferRes . inferResult . _2)
        V.BLeaf (V.LVar v) ->
            ( v ~~> [pl ^. entityId]
            , V.LVar v & V.BLeaf & Ann resPl
            )
        x ->
            htraverse (Proxy @SugarInput #> prep iScope locals) x
            <&> Ann resPl
        where
            resPl = pl & localsInScope <>~ locals & inferScope .~ iScope

instance SugarInput (HCompose Prune Type)
instance SugarInput (HCompose Prune Row)
