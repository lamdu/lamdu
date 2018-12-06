-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferred, stored, evalResults, userData
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , AnnotationMode(..), _Evaluation, _Types, _None
    , inferredType, inferredScope
    , preparePayloads
    ) where

import           AST (Ann(..), monoChildren)
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValP)
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ER.ScopeId (ER.Val Type)
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val Type)]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferred :: Infer.Payload
    , _stored :: ValP m
    , _evalResults :: CurAndPrev EvalResultsForExpr
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)

data AnnotationMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload
Lens.makePrisms ''AnnotationMode

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
        go (Ann (x, mkPayload) body) =
            ( childrenVars
              & case body of
                V.BLeaf (V.LVar var) -> Lens.at var <>~ Just [x]
                V.BLam (V.Lam var _) -> Lens.at var .~ Nothing
                _ -> id
            , b & monoChildren %~ (^. Lens._Wrapped . _2)
              & Ann
                ( case body of
                  V.BLam (V.Lam var _) -> childrenVars ^. Lens.ix var
                  _ -> []
                  & mkPayload
                )
            )
            where
                childrenVars = Map.unionsWith (++) (b ^.. monoChildren . Lens._Wrapped . _1)
                b = body & monoChildren %~ Lens.Const . go
