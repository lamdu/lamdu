{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable, RecordWildCards #-}
module Lamdu.Sugar.Types.Binder
    ( -- Annotations
      EvaluationResult
    , Annotation(..), aInferredType, aMEvaluationResult
    -- Tags
    , Tag(..), tagName, tagInfo, tagActions
    , TagInfo(..), tagVal, tagInstance
    , TagActions(..), taOptions, taChangeTag, taSetPublished, taReplaceWithNew
    -- Let
    , LetFloatResult(..)
    , LetActions(..)
        , laSetToInner, laSetToHole, laFloat, laWrap
    , Let(..)
        , lEntityId, lValue, lName, lUsages
        , lActions, lAnnotation, lBodyScope, lBody
    , ChildScopeMapping
    -- Binders
    , BinderMode(..)
    , VarToTags(..), TagsToVar(..)
    , ParamDelResult(..), ParamAddResult(..)
    , FuncParamActions(..), fpAddNext, fpDelete, fpMOrderBefore, fpMOrderAfter
    , VarParamInfo(..), vpiName, vpiActions, vpiId
    , FieldParamInfo(..), fpiActions, fpiTag
    , FuncParam(..), fpInfo, fpAnnotation
    , Anchors.PresentationMode(..)
    , Anchors.DefinitionState(..)
    , BinderActions(..), baAddFirstParam
    , NullParamActions(..), npDeleteLambda
    , BinderParams(..)
        , _BinderWithoutParams, _NullParam, _VarParam , _FieldParams
    , BinderParamScopeId(..), bParamScopeId
    , BinderBody(..), bbAddOuterLet, bbContent
    , BinderContent(..), _BinderLet, _BinderExpr
    , BinderBodyScope(..)
    , Binder(..)
        , bMPresentationModeProp, bChosenScopeProp, bParams, bBody
        , bActions, bBodyScopes
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Data.Store.Transaction (Transaction, MkProperty)
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

type T = Transaction

type EvaluationResult = Map ER.ScopeId (ER.Val Type)

data Annotation = Annotation
    { _aInferredType :: Type
    , _aMEvaluationResult :: CurAndPrev (Maybe EvaluationResult)
    } deriving Show

data VarToTags = VarToTags
    { vttReplacedVar :: V.Var
    , vttReplacedVarEntityId :: EntityId
      -- Since this is just a result of a transaction, no name is
      -- actually needed in the Tags below
    , vttReplacedByTag :: TagInfo
    , vttNewTag :: TagInfo
    }

data ParamAddResult
    = ParamAddResultNewVar EntityId V.Var
    | ParamAddResultVarToTags VarToTags
    | ParamAddResultNewTag TagInfo

data TagsToVar = TagsToVar
    { ttvReplacedTag :: TagInfo
    , ttvReplacedByVar :: V.Var
    , ttvReplacedByVarEntityId :: EntityId
    }

data ParamDelResult
    = ParamDelResultDelVar
    | ParamDelResultTagsToVar TagsToVar
    | ParamDelResultDelTag

data FuncParamActions m =
    FuncParamActions
    { _fpAddNext :: T m ParamAddResult
    , _fpDelete :: T m ParamDelResult
    , _fpMOrderBefore :: Maybe (T m ())
    , _fpMOrderAfter :: Maybe (T m ())
    }

newtype NullParamActions m = NullParamActions
    { _npDeleteLambda :: T m ()
    }

data VarParamInfo name m = VarParamInfo
    { _vpiName :: name
    , _vpiId :: EntityId
    , _vpiActions :: FuncParamActions m
    }

data FieldParamInfo name m = FieldParamInfo
    { _fpiTag :: Tag name m
    , _fpiActions :: FuncParamActions m
    }

data FuncParam info = FuncParam
    { _fpAnnotation :: Annotation
    , _fpInfo :: info
    } deriving (Functor, Foldable, Traversable)

data TagInfo = TagInfo
    { _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    } deriving (Eq, Ord, Show)

data TagActions name m = TagActions
    { _taOptions :: T m [(name, T.Tag)]
    , _taChangeTag :: T.Tag -> T m TagInfo
    , _taSetPublished :: Bool -> T m ()
    , _taReplaceWithNew :: T m TagInfo
    }

data Tag name m = Tag
    { _tagInfo :: TagInfo
    , _tagName :: name
    , _tagActions :: TagActions name m
    }

data BinderMode = NormalBinder | LightLambda

instance Show name => Show (VarParamInfo name m) where
    show VarParamInfo{..} =
        "(VarParamInfo " ++ show _vpiName ++ ")"

instance Show info => Show (FuncParam info) where
    show FuncParam{..} =
        "(FuncParam " ++ show _fpInfo ++
        " " ++ show _fpAnnotation ++ " )"


data LetFloatResult = LetFloatResult
    { lfrNewEntity :: EntityId
    , lfrMVarToTags :: Maybe VarToTags
    }

data LetActions m = LetActions
    { _laSetToInner :: T m ()
    , _laSetToHole :: T m EntityId
    , _laFloat :: T m LetFloatResult
    , _laWrap :: T m (UUID, EntityId)
    }

-- This is a mapping from a parent scope to the inner scope in:
-- * A redex lambda body (executed exactly once)
-- * Also used for guard-sugar where guard scopes are executed no more than once
type ChildScopeMapping = CurAndPrev (Map ER.ScopeId ER.ScopeId)

data Let name m expr = Let
    { _lValue :: Binder name m expr -- "let [[foo = bar]] in x"
    , _lEntityId :: EntityId
    , _lUsages :: [EntityId]
    , _lAnnotation :: Annotation
    , _lName :: name
    , _lActions :: LetActions m
    , _lBodyScope :: ChildScopeMapping
    , _lBody :: BinderBody name m expr -- "let foo = bar in [[x]]"
    }

newtype BinderActions m = BinderActions
    { _baAddFirstParam :: T m ParamAddResult
    }

data BinderParams name m
    = -- a definition or let-item without parameters
      BinderWithoutParams
    | -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam (NullParamActions m))
    | VarParam (FuncParam (VarParamInfo name m))
    | FieldParams [FuncParam (FieldParamInfo name m)]

data BinderContent name m expr
    = BinderLet (Let name m expr)
    | BinderExpr expr

data BinderBody name m expr = BinderBody
    { _bbAddOuterLet :: T m EntityId
    , _bbContent :: BinderContent name m expr
    }

data BinderBodyScope
    = SameAsParentScope
      -- ^ no binder params
    | BinderBodyScope (CurAndPrev (Map ER.ScopeId [BinderParamScopeId]))
      -- ^ binder has params, use the map to get the param application
      -- scopes

data Binder name m expr = Binder
    { _bMPresentationModeProp :: Maybe (MkProperty m Anchors.PresentationMode)
    , _bChosenScopeProp :: MkProperty m (Maybe BinderParamScopeId)
    , _bParams :: BinderParams name m
    , _bBody :: BinderBody name m expr
    , _bActions :: BinderActions m
    , -- The scope inside a lambda (if exists)
      _bBodyScopes :: BinderBodyScope
    }

Lens.makeLenses ''Annotation
Lens.makeLenses ''Binder
Lens.makeLenses ''BinderActions
Lens.makeLenses ''BinderBody
Lens.makeLenses ''FieldParamInfo
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''Let
Lens.makeLenses ''LetActions
Lens.makeLenses ''NullParamActions
Lens.makeLenses ''Tag
Lens.makeLenses ''TagActions
Lens.makeLenses ''TagInfo
Lens.makeLenses ''VarParamInfo
Lens.makePrisms ''BinderContent
Lens.makePrisms ''BinderParams

-- Manual instances to work around GHC <=8.0.2 bug causing long compile times
-- IIRC it was due to inlining recursions (TODO: find bug# / link).
-- Can replace this with automatic derivations when the bug is fixed.

instance Functor (Binder name m) where
    fmap = (bBody . Lens.mapped %~)

instance Functor (BinderBody name m) where
    fmap = (bbContent . Lens.mapped %~)

instance Functor (BinderContent name m) where
    fmap f (BinderLet l) = l <&> f & BinderLet
    fmap f (BinderExpr expr) = f expr & BinderExpr

instance Functor (Let name m) where
    fmap f l =
        l
        { _lValue = _lValue l <&> f
        , _lBody = _lBody l <&> f
        }

instance Foldable (Binder name m) where
    foldMap f x = foldMap f (x ^. bBody)

instance Foldable (BinderBody name m) where
    foldMap f x = foldMap f (x ^. bbContent)

instance Foldable (BinderContent name m) where
    foldMap f (BinderLet l) = foldMap f l
    foldMap f (BinderExpr expr) = f expr

instance Foldable (Let name m) where
    foldMap f l = foldMap f (l ^. lValue) <> foldMap f (l ^. lBody)

instance Traversable (Binder name m) where
    sequenceA = bBody sequenceA

instance Traversable (BinderBody name m) where
    sequenceA = bbContent sequenceA

instance Traversable (BinderContent name m) where
    sequenceA (BinderLet l) = sequenceA l <&> BinderLet
    sequenceA (BinderExpr expr) = expr <&> BinderExpr

instance Traversable (Let name m) where
    sequenceA l =
        f
        <$> sequenceA (l ^. lValue)
        <*> sequenceA (l ^. lBody)
        where
            f v b =
                l
                { _lValue = v
                , _lBody = b
                }
