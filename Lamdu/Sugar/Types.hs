{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RankNTypes #-}
module Lamdu.Sugar.Types
  ( EntityId
  , Definition(..), drEntityId, drName, drBody
  , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions
  , DefinitionExpression(..), deContent, deTypeInfo
  , AcceptNewType(..)
  , DefinitionTypeInfo(..)
    , _DefinitionExportedTypeInfo
    , _DefinitionNewType
  , Anchors.PresentationMode(..)
  , BinderActions(..)
    , baAddFirstParam, baAddInnermostWhereItem
  , Binder(..)
    , dSetPresentationMode, dParams, dBody, dWhereItems, dMActions
  , DefinitionBuiltin(..)
  , WrapAction(..), _WrapperAlready, _WrappedAlready, _WrapNotAllowed, _WrapAction
  , SetToHole(..), _SetToHole, _AlreadyAHole
  , SetToInnerExpr(..), _SetToInnerExpr, _NoInnerExpr
  , Actions(..)
    , wrap, setToHole, setToInnerExpr, cut
  , Body(..)
    , _BodyLam, _BodyApply, _BodyGetVar, _BodyGetField, _BodyHole
    , _BodyLiteralInteger, _BodyList, _BodyRecord
  , Payload(..), plEntityId, plInferredType, plActions, plData
  , ExpressionP(..), rBody, rPayload
  , DefinitionU
  , Expression
  , WhereItem(..), wiEntityId, wiValue, wiName, wiActions, wiInferredType
  , ListItem(..), liMActions, liExpr
  , ListActions(..), List(..)
  , RecordField(..), rfMDelete, rfTag, rfExpr
  , RecordTail(..), _RecordExtending, _ClosedRecord
  , Record(..), rItems, rMAddField, rTail
  , GetField(..), gfRecord, gfTag
  , GetVarType(..)
  , GetVar(..), gvName, gvJumpTo, gvVarType
  , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
  , AnnotatedArg(..), aaTag, aaTagExprEntityId, aaExpr
  , Apply(..), aFunc, aSpecialArgs, aAnnotatedArgs
  , FuncParamType(..)
  , FuncParam(..)
    , fpName, fpId, fpVarKind, fpInferredType, fpMActions, fpHiddenIds
  , Unwrap(..), _UnwrapMAction, _UnwrapTypeMismatch
  , HoleArg(..), haExpr, haUnwrap
  , HoleSuggested(..), hsValue, hsMakeConverted
  , Hole(..)
    , holeMActions, holeMArg, holeSuggested, holeGuid
  , ScopeItem(..), siGetVar, siVal
  , HoleActions(..)
    , holeScope, holePaste, holeResults
  , HoleResultScore
  , HoleResult(..)
    , holeResultConverted
    , holeResultPick
    , holeResultHasHoles
  , IsInjected(..)
  , PickedResult(..), prMJumpTo, prIdTranslation
  , TagG(..), tagGName, tagVal, tagInstance
  ) where

import Control.Monad.ListT (ListT)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (Traversable)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val)
import Lamdu.Sugar.Internal.EntityId (EntityId)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.Type as T

type T = Transaction

data WrapAction m
  = WrapperAlready (Guid, EntityId) -- I'm an apply-of-hole, (Guid,EntityId of hole), no need to wrap
  | WrappedAlready (Guid, EntityId) -- I'm an arg of apply-of-hole (Guid,EntityId of hole), no need to wrap
  | WrapNotAllowed -- I'm already wrapped or a tag or a hole
  | WrapAction (T m (Guid, EntityId)) -- Wrap me!

data SetToHole m
  = SetToHole (T m (Guid, EntityId))
  | AlreadyAHole -- or already an arg of one

data SetToInnerExpr m = SetToInnerExpr (T m EntityId) | NoInnerExpr

data Actions m = Actions
  { _wrap :: WrapAction m
  , _setToHole :: SetToHole m
  , _setToInnerExpr :: SetToInnerExpr m
  , _cut :: T m EntityId
  }

data Payload m a = Payload
  { _plInferredType :: Type
  , _plActions :: Maybe (Actions m)
  , _plEntityId :: EntityId
  , _plData :: a
  } deriving (Functor, Foldable, Traversable)

data ExpressionP name m pl = Expression
  { _rBody :: Body name m (ExpressionP name m pl)
  , _rPayload :: pl
  } deriving (Functor, Foldable, Traversable)

type Expression name m a = ExpressionP name m (Payload m a)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m EntityId
  , _itemDelete :: T m EntityId
  }

newtype FuncParamActions m = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  }

data FuncParamType = FuncParameter | FuncFieldParameter

-- TODO:
data FuncParam name m = FuncParam
  { _fpId :: EntityId
  , _fpVarKind :: FuncParamType
  , _fpName :: name
  , _fpInferredType :: Type
  , _fpMActions :: Maybe (FuncParamActions m)
  , -- Sometimes the Lambda disappears in Sugar, the Param "swallows" its id
    _fpHiddenIds :: [EntityId]
  }

data TagG name = TagG
  { _tagInstance :: EntityId -- Unique across different uses of a tag
  , _tagVal :: T.Tag
  , _tagGName :: name
  }

data PickedResult = PickedResult
  { _prMJumpTo :: Maybe (Guid, EntityId) -- Hole identifier within
  , -- pairs of ids from converted expression and written expression.
    _prIdTranslation :: [(EntityId, EntityId)]
  }

data IsInjected = Injected | NotInjected

instance Monoid IsInjected where
  mempty = NotInjected
  mappend NotInjected NotInjected = NotInjected
  mappend _ _ = Injected

type HoleResultScore = [Int]

data HoleResult name m = HoleResult
  { _holeResultConverted :: Expression name m IsInjected
  , _holeResultPick :: T m PickedResult
  , _holeResultHasHoles :: Bool
  }

data ScopeItem name m = ScopeItem
  { _siGetVar :: GetVar name m
  , _siVal :: Val ()
  }

data HoleActions name m = HoleActions
  { _holeScope :: T m [ScopeItem name m]
  , _holeResults ::
      Val () -> ListT (T m) (HoleResultScore, T m (HoleResult name m))
  , _holePaste :: Maybe (T m EntityId)

  , _holeGuid :: Guid -- TODO: Replace this with a way to associate data?
  }

data Unwrap m
  = UnwrapMAction (Maybe (T m EntityId))
  | UnwrapTypeMismatch

data HoleArg m expr = HoleArg
  { _haExpr :: expr
  , _haUnwrap :: Unwrap m
  } deriving (Functor, Foldable, Traversable)

data HoleSuggested name m = HoleSuggested
  { _hsValue :: Val ()
  , _hsMakeConverted :: T m (Expression name m ())
  }

data Hole name m expr = Hole
  { _holeMActions :: Maybe (HoleActions name m)
  , _holeSuggested :: HoleSuggested name m
  , _holeMArg :: Maybe (HoleArg m expr)
  } deriving (Functor, Foldable, Traversable)

data ListItem m expr = ListItem
  { _liMActions :: Maybe (ListItemActions m)
  , _liExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data ListActions m = ListActions
  { addFirstItem :: T m EntityId
  , replaceNil :: T m EntityId
  }

data List m expr = List
  { lValues :: [ListItem m expr]
  , lMActions :: Maybe (ListActions m)
  , -- Nil EntityId stays consistent when adding items.
    -- (Exposed for consistent animations)
    lNilEntityId :: EntityId
  } deriving (Functor, Foldable, Traversable)

data RecordField name m expr = RecordField
  { _rfMDelete :: Maybe (T m EntityId)
  , _rfTag :: TagG name
  , _rfExpr :: expr -- field type or val
  } deriving (Functor, Foldable, Traversable)

data RecordTail m expr =
  RecordExtending expr |
  ClosedRecord (Maybe (T m EntityId)) -- delete action
  deriving (Functor, Foldable, Traversable)

data Record name m expr = Record
  { _rItems :: [RecordField name m expr]
  , _rTail :: RecordTail m expr
  , _rMAddField :: Maybe (T m EntityId)
  } deriving (Functor, Foldable, Traversable)

data GetField name expr = GetField
  { _gfRecord :: expr
  , _gfTag :: TagG name
  } deriving (Functor, Foldable, Traversable)

data GetVarType = GetDefinition | GetFieldParameter | GetParameter | GetParamsRecord
  deriving (Eq, Ord)

data GetVar name m = GetVar
  { _gvName :: name
  , _gvJumpTo :: T m EntityId
  , _gvVarType :: GetVarType
  }

data SpecialArgs expr
  = NoSpecialArgs
  | ObjectArg expr
  | InfixArgs expr expr
  deriving (Functor, Foldable, Traversable)

data AnnotatedArg name expr = AnnotatedArg
  { _aaTag :: TagG name
  , -- Used for animation ids consistent with record.
    _aaTagExprEntityId :: EntityId
  , _aaExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data Apply name expr = Apply
  { _aFunc :: expr
  , _aSpecialArgs :: SpecialArgs expr
  , _aAnnotatedArgs :: [AnnotatedArg name expr]
  } deriving (Functor, Foldable, Traversable)

data Body name m expr
  = BodyLam (Binder name m expr)
  | BodyApply (Apply name expr)
  | BodyHole (Hole name m expr)
  | BodyLiteralInteger Integer
  | BodyList (List m expr)
  | BodyRecord (Record name m expr)
  | BodyGetField (GetField name expr)
  | BodyGetVar (GetVar name m)
  deriving (Functor, Foldable, Traversable)

instance Show (FuncParam name m) where
  show _fp = "TODO:FuncParam"

instance Show expr => Show (Body name m expr) where
  show (BodyLam _) = "TODO show lam"
  show BodyHole {} = "Hole"
  show (BodyLiteralInteger i) = show i
  show (BodyList (List items _ _)) =
    concat
    [ "["
    , List.intercalate ", " $ map (show . _liExpr) items
    , "]"
    ]
  show BodyApply {} = "LabelledApply:TODO"
  show BodyRecord {} = "Record:TODO"
  show BodyGetField {} = "GetField:TODO"
  show BodyGetVar {} = "GetVar:TODO"

data WhereItem name m expr = WhereItem
  { _wiValue :: Binder name m expr
  , _wiEntityId :: EntityId
  , _wiInferredType :: Type
  , _wiName :: name
  , _wiActions :: Maybe (ListItemActions m)
  } deriving (Functor, Foldable, Traversable)

data BinderActions m = BinderActions
  { _baAddFirstParam :: T m EntityId
  , _baAddInnermostWhereItem :: T m EntityId
  }

data Binder name m expr = Binder
  { _dSetPresentationMode :: Maybe (MkProperty m Anchors.PresentationMode)
  , _dParams :: [FuncParam name m]
  , _dBody :: expr
  , _dWhereItems :: [WhereItem name m expr]
  , _dMActions :: Maybe (BinderActions m)
  } deriving (Functor, Foldable, Traversable)

data AcceptNewType m = AcceptNewType
  { antOldType :: Definition.ExportedType
  , antNewType :: Scheme
  , antAccept :: T m ()
  }

data DefinitionTypeInfo m
  = DefinitionExportedTypeInfo Scheme
  | DefinitionNewType (AcceptNewType m)

data DefinitionExpression name m expr = DefinitionExpression
  { _deTypeInfo :: DefinitionTypeInfo m
  , _deContent :: Binder name m expr
  } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin m = DefinitionBuiltin
  { biName :: Definition.FFIName
  , biSetName :: Definition.FFIName -> T m ()
  , biType :: Scheme
  }

data DefinitionBody name m expr
  = DefinitionBodyExpression (DefinitionExpression name m expr)
  | DefinitionBodyBuiltin (DefinitionBuiltin m)
  deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
  { _drName :: name
  , _drEntityId :: EntityId
  , _drBody :: DefinitionBody name m expr
  } deriving (Functor, Foldable, Traversable)

type DefinitionU m a = Definition Guid m (Expression Guid m a)

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Apply
Lens.makeLenses ''Binder
Lens.makeLenses ''BinderActions
Lens.makeLenses ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''ExpressionP
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''GetField
Lens.makeLenses ''GetVar
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleActions
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleResult
Lens.makeLenses ''HoleSuggested
Lens.makeLenses ''ListItem
Lens.makeLenses ''ListItemActions
Lens.makeLenses ''Payload
Lens.makeLenses ''PickedResult
Lens.makeLenses ''Record
Lens.makeLenses ''RecordField
Lens.makeLenses ''ScopeItem
Lens.makeLenses ''TagG
Lens.makeLenses ''WhereItem
Lens.makePrisms ''Body
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''DefinitionTypeInfo
Lens.makePrisms ''RecordTail
Lens.makePrisms ''SetToHole
Lens.makePrisms ''SetToInnerExpr
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''Unwrap
Lens.makePrisms ''WrapAction
