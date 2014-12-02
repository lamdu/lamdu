{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RankNTypes, DeriveGeneric #-}
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
  , DefinitionContent(..)
    , dSetPresentationMode, dParams, dBody, dWhereItems
    , dAddFirstParam, dAddInnermostWhereItem
  , DefinitionBuiltin(..)
  , WrapAction(..)
  , SetToHole(..), _SetToHole, _AlreadyAHole
  , SetToInnerExpr(..), _SetToInnerExpr, _NoInnerExpr
  , Actions(..)
    , wrap, setToHole, setToInnerExpr, cut
  , Body(..)
    , _BodyLam, _BodyApply, _BodyGetVar, _BodyGetField, _BodyHole
    , _BodyLiteralInteger, _BodyList, _BodyRecord
  , Payload(..), plEntityId, plInferredType, plActions, plData
  , ExpressionP(..), rBody, rPayload
  , NameSource(..), NameCollision(..), Name(..), MStoredName
  , DefinitionN, DefinitionU
  , Expression, ExpressionN
  , BodyN
  , WhereItem(..), wiEntityId, wiValue, wiName, wiActions, wiInferredType
  , ListItem(..), liMActions, liExpr
  , ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Record(..), rItems, rMAddFirstItem
  , GetField(..), gfRecord, gfTag
  , GetVarType(..)
  , GetVar(..), gvName, gvJumpTo, gvVarType
  , GetParams(..), gpDefName, gpJumpTo
  , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
  , AnnotatedArg(..), aaTag, aaTagExprEntityId, aaExpr
  , Apply(..), aFunc, aSpecialArgs, aAnnotatedArgs
  , Lam(..), lParam, lResult
  , FuncParamType(..)
  , FuncParam(..)
    , fpName, fpId, fpVarKind, fpInferredType, fpMActions
  , Unwrap(..), _UnwrapMAction, _UnwrapTypeMismatch
  , HoleArg(..), haExpr, haExprPresugared, haUnwrap
  , HoleSuggested(..), hsSuggestedValue, hsType, hsMakeConverted
  , Hole(..)
    , holeMActions, holeMArg, holeSuggested, holeGuid
  , ScopeItem
  , Scope(..), scopeLocals, scopeGlobals, scopeTags, scopeGetParams
  , HoleActions(..)
    , holeScope, holePaste, holeInferExprType
  , HoleResult(..)
    , holeResultComplexityScore
    , holeResultConverted
    , holeResultPick
    , holeResultHasHoles
  , PickedResult(..), prMJumpTo, prIdTranslation
  , TagG(..), tagGName, tagVal, tagInstance
  , MStorePoint, ExprStorePoint
  -- Input types:
  , InputPayload(..), ipGuid, ipEntityId, ipInferred, ipStored, ipData
  , NameProperty(..)
    , npName, npGuid, npSetName
  ) where

import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Data.Monoid.Generic (def_mempty, def_mappend)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val)
import Lamdu.Sugar.Internal.EntityId (EntityId)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Types.Internal as TypesInternal
import qualified System.Random as Random

type T = Transaction

data InputPayload m a
  = InputPayload
    { _ipEntityId :: EntityId
    , -- Used as a hole id that later GUI uses to associate data with
      -- Need to replace this with some mechanism that avoids exposing
      -- Guids to GUI
      _ipGuid :: Guid
    , _ipInferred :: Infer.Payload
    , _ipStored :: Maybe (ExprIRef.ValIProperty m)
    , _ipData :: a
    }
Lens.makeLenses ''InputPayload

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
  -- This must be embedded in the expression AST and not as a separate
  -- function so that AddNames can correct the "name" here in the
  -- right context.
  , _plActions :: Maybe (Actions m)
  , _plEntityId :: EntityId
  , _plData :: a
  } deriving (Functor, Foldable, Traversable)

-- When fabricating a new hole result involving a stored argument,
-- this Maybe varies between Nothing and Just in the same expression
type MStorePoint m a = (Maybe (TypesInternal.StorePoint (Tag m)), a)

type ExprStorePoint m a = Val (MStorePoint m a)

data ExpressionP name m pl = Expression
  { _rBody :: Body name m (ExpressionP name m pl)
  , _rPayload :: pl
  } deriving (Functor, Foldable, Traversable)

data NameSource = NameSourceAutoGenerated | NameSourceStored
  deriving (Show)
data NameCollision = NoCollision | Collision {-Disambiguator:-} Int
  deriving (Show)
data Name = Name
  { nNameSource :: NameSource
  , nNameCollisionSuffix :: NameCollision
  , nName :: String
  } deriving (Show)

-- This funny type-alias exists because MStoredName is an extremely
-- common type parameter of virtually all sugar types. Using (Maybe
-- StoredName) or (Maybe String) directly adds a lot of noise.
type MStoredName = Maybe String

type Expression name m a = ExpressionP name m (Payload m a)
type ExpressionN m a = Expression Name m a

type BodyN m a = Body Name m (ExpressionN m a)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m EntityId
  , _itemDelete :: T m EntityId
  }

newtype FuncParamActions m = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  }

data FuncParamType = FuncParameter | FuncFieldParameter

data NameProperty name m = NameProperty
  { _npName :: name
  , _npGuid :: Guid
  , _npSetName :: String -> T m ()
  }

-- TODO:
data FuncParam name m = FuncParam
  { _fpId :: EntityId
  , _fpVarKind :: FuncParamType
  , _fpName :: NameProperty name m
  , _fpInferredType :: Type
  , _fpMActions :: Maybe (FuncParamActions m)
  }

data Lam name m expr = Lam
  { _lParam :: FuncParam name m
  , _lResult :: expr
  } deriving (Functor, Foldable, Traversable)

data TagG name m = TagG
  { _tagInstance :: EntityId -- Unique across different uses of a tag
  , _tagVal :: T.Tag
  , _tagGName :: NameProperty name m
  }

data PickedResult = PickedResult
  { _prMJumpTo :: Maybe (Guid, EntityId) -- Hole identifier within
  , -- pairs of ids from converted expression and written expression.
    _prIdTranslation :: [(EntityId, EntityId)]
  }

data HoleResult name m a = HoleResult
  { _holeResultComplexityScore :: [Int]
  , _holeResultConverted :: Expression name m a
  , _holeResultPick :: T m PickedResult
  , _holeResultHasHoles :: Bool
  } deriving (Functor, Foldable, Traversable)

type ScopeItem a = (a, Val ())

data Scope name m = Scope
  { _scopeLocals    :: [ScopeItem (GetVar name m)]
  , _scopeGlobals   :: [ScopeItem (GetVar name m)]
  , _scopeTags      :: [(TagG name m, T.Tag)]
  , _scopeGetParams :: [ScopeItem (GetParams name m)]
  } deriving (Generic)
instance Monoid (Scope name m) where
  mempty = def_mempty
  mappend = def_mappend

data HoleActions name m = HoleActions
  { _holeScope :: T m (Scope name m)
  , -- Infer expression "on the side" (not in the hole position),
    -- but with the hole's scope.
    -- If given expression does not type check on its own, returns Nothing.
    -- (used by HoleEdit to suggest variations based on type)
    _holeInferExprType :: Val () -> T m (Maybe Type)
  , holeResult ::
      forall a. Monoid a =>
      Val (MStorePoint m a) -> T m (Maybe (HoleResult name m a))
  , _holePaste :: Maybe (T m EntityId)

  , _holeGuid :: Guid -- TODO: Replace this with a way to associate data?
  }

data Unwrap m
  = UnwrapMAction (Maybe (T m EntityId))
  | UnwrapTypeMismatch

data HoleArg m expr = HoleArg
  { _haExpr :: expr
  , _haExprPresugared :: ExprStorePoint m ()
  , _haUnwrap :: Unwrap m
  } deriving (Functor, Foldable, Traversable)

data HoleSuggested name m = HoleSuggested
  { _hsSuggestedValue :: Val ()
  , _hsType :: Type
  , _hsMakeConverted :: Random.StdGen -> T m (Expression name m ())
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
  { _rfMItemActions :: Maybe (ListItemActions m)
  , _rfTag :: TagG name m
  , _rfExpr :: expr -- field type or val
  } deriving (Functor, Foldable, Traversable)

data Record name m expr = Record
  { _rItems :: [RecordField name m expr]
  , _rMAddFirstItem :: Maybe (T m EntityId)
  } deriving (Functor, Foldable, Traversable)

data GetField name m expr = GetField
  { _gfRecord :: expr
  , _gfTag :: TagG name m
  } deriving (Functor, Foldable, Traversable)

data GetVarType = GetDefinition | GetFieldParameter | GetParameter
  deriving (Eq, Ord)

data GetVar name m = GetVar
  { _gvName :: NameProperty name m
  , _gvJumpTo :: T m EntityId
  , _gvVarType :: GetVarType
  }

data GetParams name m = GetParams
  { _gpDefName :: NameProperty name m
  , _gpJumpTo :: T m EntityId
  }

data SpecialArgs expr
  = NoSpecialArgs
  | ObjectArg expr
  | InfixArgs expr expr
  deriving (Functor, Foldable, Traversable)

data AnnotatedArg name m expr = AnnotatedArg
  { _aaTag :: TagG name m
  , -- Used for animation ids consistent with record.
    _aaTagExprEntityId :: EntityId
  , _aaExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data Apply name m expr = Apply
  { _aFunc :: expr
  , _aSpecialArgs :: SpecialArgs expr
  , _aAnnotatedArgs :: [AnnotatedArg name m expr]
  } deriving (Functor, Foldable, Traversable)

data Body name m expr
  = BodyLam (Lam name m expr)
  | BodyApply (Apply name m expr)
  | BodyHole (Hole name m expr)
  | BodyLiteralInteger Integer
  | BodyList (List m expr)
  | BodyRecord (Record name m expr)
  | BodyGetField (GetField name m expr)
  | BodyGetVar (GetVar name m)
  | BodyGetParams (GetParams name m)
  deriving (Functor, Foldable, Traversable)

instance Show (FuncParam name m) where
  show _fp = "TODO:FuncParam"

instance Show expr => Show (Body name m expr) where
  show (BodyLam (Lam paramType resultType)) =
    "_:" ++ show paramType ++ " -> " ++ show resultType
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
  show BodyGetParams {} = "GetParams:TODO"

data WhereItem name m expr = WhereItem
  { _wiValue :: DefinitionContent name m expr
  , _wiEntityId :: EntityId
  , _wiInferredType :: Type
  , _wiName :: NameProperty name m
  , _wiActions :: Maybe (ListItemActions m)
  } deriving (Functor, Foldable, Traversable)

-- Common data for definitions and where-items
data DefinitionContent name m expr = DefinitionContent
  { _dSetPresentationMode :: Maybe (MkProperty m Anchors.PresentationMode)
  , _dParams :: [FuncParam name m]
  , _dBody :: expr
  , _dWhereItems :: [WhereItem name m expr]
  , _dAddFirstParam :: T m EntityId
  , _dAddInnermostWhereItem :: T m EntityId
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
  , _deContent :: DefinitionContent name m expr
  } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin m = DefinitionBuiltin
  { biName :: Definition.FFIName
  , biSetName :: Definition.FFIName -> T m ()
  , biType :: Definition.ExportedType
  }

data DefinitionBody name m expr
  = DefinitionBodyExpression (DefinitionExpression name m expr)
  | DefinitionBodyBuiltin (DefinitionBuiltin m)
  deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
  { _drName :: NameProperty name m
  , _drEntityId :: EntityId
  , _drBody :: DefinitionBody name m expr
  } deriving (Functor, Foldable, Traversable)

type DefinitionN m a = Definition Name m (Expression Name m a)
type DefinitionU m a = Definition MStoredName m (Expression MStoredName m a)

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Apply
Lens.makeLenses ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionContent
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''ExpressionP
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''GetField
Lens.makeLenses ''GetParams
Lens.makeLenses ''GetVar
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleActions
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleSuggested
Lens.makeLenses ''HoleResult
Lens.makeLenses ''Lam
Lens.makeLenses ''ListItem
Lens.makeLenses ''ListItemActions
Lens.makeLenses ''Payload
Lens.makeLenses ''PickedResult
Lens.makeLenses ''Record
Lens.makeLenses ''RecordField
Lens.makeLenses ''Scope
Lens.makeLenses ''TagG
Lens.makeLenses ''WhereItem
Lens.makeLenses ''NameProperty
Lens.makePrisms ''Body
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''DefinitionTypeInfo
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''Unwrap
Lens.makePrisms ''SetToHole
Lens.makePrisms ''SetToInnerExpr
