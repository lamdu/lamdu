{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Lamdu.Sugar.Types.Binder
    ( -- Annotations
      EvaluationResult
    , Annotation(..), aInferredType, aMEvaluationResult
    -- Node actions
    , DetachAction(..), _FragmentAlready, _FragmentExprAlready, _DetachAction
    , NodeActions(..), detach, mSetToHole, extract, mReplaceParent, wrapInRecord
    -- Let
    , ExtractDestination(..)
    , LetActions(..), laDelete, laNodeActions
    , Let(..)
        , lEntityId, lValue, lName, lUsages
        , lActions, lAnnotation, lBodyScope, lBody
    , ChildScopeMapping
    -- Binders
    , AddNextParam(..), _AddNext, _NeedToPickTagToAddNext
    , FuncParamActions(..), fpAddNext, fpDelete, fpMOrderBefore, fpMOrderAfter
    , ParamInfo(..), piActions, piTag
    , FuncParam(..), fpInfo, fpAnnotation
    , Meta.SpecialArgs(..)
    , Meta.DefinitionState(..)
    , BinderActions(..), baAddFirstParam, baMNodeActions
    , NullParamActions(..), npDeleteLambda
    , AddFirstParam(..), _AddInitialParam, _PrependParam, _NeedToPickTagToAddFirst
    , BinderParams(..), _BinderWithoutParams, _NullParam, _Params
    , BinderParamScopeId(..), bParamScopeId
    , BinderBody(..), bbAddOuterLet, bbContent
    , BinderContent(..), _BinderLet, _BinderExpr
    , BinderBodyScope(..)
    , Binder(..)
        , bChosenScopeProp, bParams, bBody
        , bLamId, bActions, bBodyScopes
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Data.Property (Property)
import           Lamdu.Calc.Type (Type)
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

type EvaluationResult = Map ER.ScopeId (ER.Val Type)

data Annotation = Annotation
    { _aInferredType :: Type
    , _aMEvaluationResult :: CurAndPrev (Maybe EvaluationResult)
    } deriving Show

data AddNextParam name m
    = AddNext (TagSelection name m ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddNext EntityId

data FuncParamActions name m =
    FuncParamActions
    { _fpAddNext :: AddNextParam name m
    , _fpDelete :: m ()
    , _fpMOrderBefore :: Maybe (m ())
    , _fpMOrderAfter :: Maybe (m ())
    }

newtype NullParamActions m = NullParamActions
    { _npDeleteLambda :: m ()
    }

data ParamInfo name m = ParamInfo
    { _piTag :: Tag name m
    , _piActions :: FuncParamActions name m
    }

data FuncParam info = FuncParam
    { _fpAnnotation :: Annotation
    , _fpInfo :: info
    } deriving (Functor, Foldable, Traversable)

instance Show info => Show (FuncParam info) where
    show FuncParam{..} =
        "(FuncParam " ++ show _fpInfo ++
        " " ++ show _fpAnnotation ++ " )"

data ExtractDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data DetachAction m
    = FragmentAlready EntityId -- I'm an apply-of-hole, no need to detach
    | FragmentExprAlready EntityId -- I'm an arg of apply-of-hole, no need to detach
    | DetachAction (m EntityId) -- Detach me

data NodeActions name m = NodeActions
    { _detach :: DetachAction m
    , _mSetToHole :: Maybe (m EntityId) -- (Not available for holes)
    , _extract :: m ExtractDestination
    , _mReplaceParent :: Maybe (m EntityId)
    , _wrapInRecord :: TagSelection name m ()
    }

data LetActions name m = LetActions
    { _laDelete :: m ()
    , _laNodeActions :: NodeActions name m
    }

-- This is a mapping from a parent scope to the inner scope in:
-- * A redex lambda body (executed exactly once)
-- * Also used for if-else sugar where else-if scopes are executed no more than once
type ChildScopeMapping = CurAndPrev (Map ER.ScopeId ER.ScopeId)

data Let name m expr = Let
    { _lValue :: Binder name m expr -- "let [[foo = bar]] in x"
    , _lEntityId :: EntityId
    , _lUsages :: [EntityId]
    , _lAnnotation :: Annotation
    , _lName :: Tag name m
    , _lActions :: LetActions name m
    , _lBodyScope :: ChildScopeMapping
    , _lBody :: BinderBody name m expr -- "let foo = bar in [[x]]"
    } deriving (Functor, Foldable, Traversable)

data AddFirstParam name m
    = -- The inital param is created with anon-tag
      AddInitialParam (m EntityId)
    | PrependParam (TagSelection name m ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddFirst EntityId

data BinderActions name m = BinderActions
    { _baAddFirstParam :: AddFirstParam name m
    , _baMNodeActions :: Maybe (NodeActions name m)
    }

data BinderParams name m
    = -- a definition or let-item without parameters
      BinderWithoutParams
    | -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam (NullParamActions m))
    | Params [FuncParam (ParamInfo name m)]

data BinderContent name m expr
    = BinderLet (Let name m expr)
    | BinderExpr expr
    deriving (Functor, Foldable, Traversable)

data BinderBody name m expr = BinderBody
    { _bbAddOuterLet :: m EntityId
    , _bbContent :: BinderContent name m expr
    } deriving (Functor, Foldable, Traversable)

data BinderBodyScope
    = SameAsParentScope
      -- ^ no binder params
    | BinderBodyScope (CurAndPrev (Map ER.ScopeId [BinderParamScopeId]))
      -- ^ binder has params, use the map to get the param application
      -- scopes

data Binder name m expr = Binder
    { _bChosenScopeProp :: m (Property m (Maybe BinderParamScopeId))
    , _bParams :: BinderParams name m
    , _bLamId :: Maybe EntityId
    , _bBody :: BinderBody name m expr
    , _bActions :: BinderActions name m
    , -- The scope inside a lambda (if exists)
      _bBodyScopes :: BinderBodyScope
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Annotation
Lens.makeLenses ''Binder
Lens.makeLenses ''BinderActions
Lens.makeLenses ''BinderBody
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''Let
Lens.makeLenses ''LetActions
Lens.makeLenses ''NodeActions
Lens.makeLenses ''NullParamActions
Lens.makeLenses ''ParamInfo
Lens.makePrisms ''AddFirstParam
Lens.makePrisms ''AddNextParam
Lens.makePrisms ''BinderContent
Lens.makePrisms ''BinderParams
Lens.makePrisms ''DetachAction
