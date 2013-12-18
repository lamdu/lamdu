{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Trigger.Types
  ( Trigger(..)
  , ParameterRefEvent(..)
  , Fired(..), _FiredDirectlyTag, _FiredKnownBody, _FiredParameterRef, _FiredUnify
  ) where

import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Lamdu.Data.Expression.Utils () -- Show instance for Expr.Body
import Lamdu.Data.Infer.RefTags (ParamRef, ExprRef)
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expr as Expr

-- Triggers are alive as long as their truthfulness is yet
-- unknown. When they become known, they fire (see Fired) below and
-- disappear.
data Trigger def
  = OnDirectlyTag
  | OnKnownBody
  | OnParameterRef (ParamRef def)
  | OnUnify
  deriving (Eq, Ord, Show)
derive makeBinary ''Trigger

data ParameterRefEvent
  = TheParameterOutOfScope
  | NotTheParameterRef
  | IsTheParameterRef
  deriving (Eq, Ord, Show)

data Fired def
  = FiredDirectlyTag Bool
  | FiredKnownBody (Expr.Body () (ExprRef def))
  | FiredParameterRef (ParamRef def) ParameterRefEvent
  | FiredUnify (ExprRef def)
  deriving (Show)
Lens.makePrisms ''Fired
