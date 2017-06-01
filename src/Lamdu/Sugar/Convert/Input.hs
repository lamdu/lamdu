-- | Preprocess of input to sugar
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferred, stored, evalResults, userData
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , inferredType, inferredScope
    , preparePayloads
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValIProperty)
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ER.ScopeId (ER.Val ())
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val ())]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferred :: Infer.Payload
    , _stored :: ValIProperty m
    , _evalResults :: CurAndPrev EvalResultsForExpr
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload

inferredType :: Lens' (Payload m a) Type
inferredType = inferred . Infer.plType

inferredScope :: Lens' (Payload m a) Infer.Scope
inferredScope = inferred . Infer.plScope

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

preparePayloads :: Val (EntityId, [EntityId] -> pl) -> Val pl
preparePayloads =
    snd . go
    where
        go :: Val (EntityId, [EntityId] -> pl) -> (Map V.Var [EntityId], Val pl)
        go (Val (x, mkPayload) body) =
            ( childrenVars
              & case body of
                V.BLeaf (V.LVar var) -> Lens.at var <>~ Just [x]
                V.BLam (V.Lam var _) -> Lens.at var .~ Nothing
                _ -> id
            , b <&> snd
              & Val
                ( case body of
                  V.BLam (V.Lam var _) -> childrenVars ^. Lens.at var . Lens._Just
                  _ -> []
                  & mkPayload
                )
            )
            where
                childrenVars = Map.unionsWith (++) (b ^.. Lens.traverse . Lens._1)
                b = body <&> go
