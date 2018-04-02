{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Lamdu.Sugar.Types.Binder
    ( -- Annotations
      Annotation(..), aInferredType, aMEvaluationResult
    -- Node actions
    , DetachAction(..), _FragmentAlready, _FragmentExprAlready, _DetachAction
    , NodeActions(..), detach, mSetToHole, extract, mReplaceParent, wrapInRecord
    -- Let
    , ExtractDestination(..)
    , LetActions(..), laDelete, laNodeActions
    , Let(..)
        , lEntityId, lValue, lName, lUsages
        , lActions, lAnnotation, lBodyScope, lBody
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
import           Data.Property (Property)
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

data Annotation name = Annotation
    { _aInferredType :: Type name
    , _aMEvaluationResult :: EvaluationScopes name
    } deriving Show

data AddNextParam name im am
    = AddNext (TagSelection name im am ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddNext EntityId

data FuncParamActions name im am =
    FuncParamActions
    { _fpAddNext :: AddNextParam name im am
    , _fpDelete :: am ()
    , _fpMOrderBefore :: Maybe (am ())
    , _fpMOrderAfter :: Maybe (am ())
    }

newtype NullParamActions am = NullParamActions
    { _npDeleteLambda :: am ()
    }

data ParamInfo name im am = ParamInfo
    { _piTag :: Tag name im am
    , _piActions :: FuncParamActions name im am
    }

data FuncParam name info = FuncParam
    { _fpAnnotation :: Annotation name
    , _fpInfo :: info
    } deriving (Functor, Foldable, Traversable)

instance (Show info, Show name) => Show (FuncParam name info) where
    show FuncParam{..} =
        "(FuncParam " ++ show _fpInfo ++
        " " ++ show _fpAnnotation ++ " )"

data ExtractDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data DetachAction am
    = FragmentAlready EntityId -- I'am an apply-of-hole, no need to detach
    | FragmentExprAlready EntityId -- I'am an arg of apply-of-hole, no need to detach
    | DetachAction (am EntityId) -- Detach me

data NodeActions name im am = NodeActions
    { _detach :: DetachAction am
    , _mSetToHole :: Maybe (am EntityId) -- (Not available for holes)
    , _extract :: am ExtractDestination
    , _mReplaceParent :: Maybe (am EntityId)
    , _wrapInRecord :: TagSelection name im am ()
    }

data LetActions name im am = LetActions
    { _laDelete :: am ()
    , _laNodeActions :: NodeActions name im am
    }

data Let name im am expr = Let
    { _lValue :: Binder name im am expr -- "let [[foo = bar]] in x"
    , _lEntityId :: EntityId
    , _lUsages :: [EntityId]
    , _lAnnotation :: Annotation name
    , _lName :: Tag name im am
    , _lActions :: LetActions name im am
    , _lBodyScope :: ChildScopes
    , _lBody :: BinderBody name im am expr -- "let foo = bar in [[x]]"
    } deriving (Functor, Foldable, Traversable)

data AddFirstParam name im am
    = -- The inital param is created with anon-tag
      AddInitialParam (am EntityId)
    | PrependParam (TagSelection name im am ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddFirst EntityId

data BinderActions name im am = BinderActions
    { _baAddFirstParam :: AddFirstParam name im am
    , _baMNodeActions :: Maybe (NodeActions name im am)
    }

data BinderParams name im am
    = -- a definition or let-item without parameters
      BinderWithoutParams
    | -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam name (NullParamActions am))
    | Params [FuncParam name (ParamInfo name im am)]

data BinderContent name im am expr
    = BinderLet (Let name im am expr)
    | BinderExpr expr
    deriving (Functor, Foldable, Traversable)

data BinderBody name im am expr = BinderBody
    { _bbAddOuterLet :: am EntityId
    , _bbContent :: BinderContent name im am expr
    } deriving (Functor, Foldable, Traversable)

data BinderBodyScope
    = SameAsParentScope
      -- ^ no binder params
    | BinderBodyScope ParamScopes
      -- ^ binder has params, use the map to get the param application
      -- scopes

data Binder name im am expr = Binder
    { _bChosenScopeProp :: im (Property am (Maybe BinderParamScopeId))
    , _bParams :: BinderParams name im am
    , _bLamId :: Maybe EntityId
    , _bBody :: BinderBody name im am expr
    , _bActions :: BinderActions name im am
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
