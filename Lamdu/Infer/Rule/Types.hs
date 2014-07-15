{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Infer.Rule.Types
  ( RuleRef
  , ExprLink(..), applyExprLinkDest, applyExprLinkDestAncestors
  , Apply(..), aPiGuid, aArgVal, aLinkedExprs, aLinkedNames
  , Uncircumsize(..), uValRef, uApplicantValRef, uUncircumsizedBody
  , Rule(..), ruleTriggersIn, ruleContent
  , RuleContent(..)
  , RuleMap(..), rmMap, rmFresh
    , verifyTagId, initialRuleMap
  , new
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT, runState)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.Infer.RefData (LoadedBody)
import Lamdu.Infer.RefTags (ExprRef, TagExpr, RuleRef, TagRule, ParamRef, TagParam)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR

data ExprLink def = ExprLink
  { _applyExprLinkDest :: ExprRef def
  , _applyExprLinkDestAncestors :: OR.RefSet (TagExpr def)
  } deriving Show
Lens.makeLenses ''ExprLink
derive makeBinary ''ExprLink

data Apply def = Apply
  { _aPiGuid :: Guid
  , _aArgVal :: ExprRef def
  -- unmaintained pi-result to apply type respective/matching subexprs
  , _aLinkedExprs :: OR.RefMap (TagExpr def) (ExprLink def)
  , _aLinkedNames :: OR.RefMap (TagParam def) (ParamRef def)
  } deriving Show
Lens.makeLenses ''Apply
derive makeBinary ''Apply

data Uncircumsize def = Uncircumsize
  { _uValRef :: ExprRef def
  , _uApplicantValRef :: ExprRef def
  , _uUncircumsizedBody :: LoadedBody def (ExprRef def)
  }

instance Show (Uncircumsize def) where
  show (Uncircumsize vr avr _b) =
    "Uncircumsize " ++ show vr ++ " " ++ show avr ++ " TODO"

Lens.makeLenses ''Uncircumsize
derive makeBinary ''Uncircumsize

data RuleContent def
  = RuleVerifyTag
  | RuleApply (Apply def)
  | RuleUncircumsize (Uncircumsize def)
  deriving Show
derive makeBinary ''RuleContent

data Rule def = Rule
  { _ruleTriggersIn :: OR.RefSet (TagExpr def)
  , _ruleContent :: RuleContent def
  } deriving Show
Lens.makeLenses ''Rule
derive makeBinary ''Rule

data RuleMap def = RuleMap
  { _rmFresh :: OR.Fresh (TagRule def)
  , _rmMap :: OR.RefMap (TagRule def) (Rule def)
  }
Lens.makeLenses ''RuleMap
derive makeBinary ''RuleMap

new :: MonadA m => RuleContent def -> StateT (RuleMap def) m (RuleRef def)
new rule = do
  ruleId <- Lens.zoom rmFresh OR.freshRef
  rmMap . Lens.at ruleId .= Just (Rule mempty rule)
  return ruleId

verifyTagId :: RuleRef def
initialRuleMap :: RuleMap def
(verifyTagId, initialRuleMap) =
  runState (new RuleVerifyTag)
  RuleMap
  { _rmFresh = OR.initialFresh
  , _rmMap = mempty
  }
