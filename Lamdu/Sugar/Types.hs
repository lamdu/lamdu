{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, DeriveDataTypeable, RankNTypes #-}
module Lamdu.Sugar.Types
  ( Definition(..), drName, drGuid, drBody
  , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), deContent, deTypeInfo
  , ShowIncompleteType(..), AcceptNewType(..)
  , DefinitionTypeInfo(..)
    , _DefinitionExportedTypeInfo
    , _DefinitionIncompleteType
    , _DefinitionNewType
  , DefinitionContent(..)
    , dDepParams, dParams, dBody, dWhereItems, dAddFirstParam, dAddInnermostWhereItem
  , DefinitionBuiltin(..)
  , WrapAction(..)
  , Actions(..)
    , storedGuid, wrap, mSetToHole, mSetToInnerExpr, cut
  , Body(..)
    , _BodyLam, _BodyApply, _BodyGetVar, _BodyGetField, _BodyHole
    , _BodyCollapsed, _BodyLiteralInteger
    , _BodyAtom, _BodyList, _BodyRecord, _BodyTag
  , Payload(..), plGuid, plInferredTypes, plActions, plData
  , ExpressionP(..), rBody, rPayload
  , NameSource(..), NameCollision(..), Name(..), MStoredName
  , DefinitionN, DefinitionU
  , Expression, ExpressionN
  , BodyN
  , WhereItem(..), wiValue, wiGuid, wiName, wiActions, wiInferredType
  , ListItem(..), liMActions, liExpr
  , ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..)
  , Record(..), rFields, rKind
  , FieldList(..), flItems, flMAddFirstItem
  , GetField(..), gfRecord, gfTag
  , GetVarType(..)
  , GetVar(..), gvIdentifier, gvName, gvJumpTo, gvVarType
  , GetParams(..), gpDefGuid, gpDefName, gpJumpTo
  , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
  , AnnotatedArg(..), aaTag, aaTagExprGuid, aaExpr
  , Apply(..), aFunc, aSpecialArgs, aAnnotatedArgs
  , Lam(..), lKind, lParam, lIsDep, lResultType
  , FuncParamType(..)
  , FuncParam(..), fpName, fpGuid, fpId, fpAltIds, fpVarKind, fpType, fpInferredType, fpMActions
  , Unwrap(..), _UnwrapMAction, _UnwrapTypeMismatch
  , HoleArg(..), haExpr, haExprPresugared, haUnwrap
  , HoleInferred(..), hiBaseValue, hiWithVarsValue, hiType, hiMakeConverted
  , Hole(..)
    , holeMActions, holeMArg, holeMInferred
  , HoleResultSeed(..), _ResultSeedExpression, _ResultSeedNewTag, _ResultSeedNewDefinition
  , ScopeItem
  , Scope(..), scopeLocals, scopeGlobals, scopeTags, scopeGetParams
  , HoleActions(..)
    , holeScope, holePaste, holeInferExprType
  , HoleResult(..)
    , holeResultInferred
    , holeResultConverted
    , holeResultPick
    , holeResultHasHoles
  , PickedResult(..), prMJumpTo, prIdTranslation
  , TagG(..), tagName, tagGuid
  , Collapsed(..), cFuncGuid, cCompact, cFullExpression, cFullExprHasInfo
  , MStorePoint, ExprStorePoint
  -- Input types:
  , InputPayloadP(..), ipGuid, ipInferred, ipStored, ipData
  , InputPayload, InputExpr
  , Stored, Inferred
  , LoadedExpr
  ) where

import Data.Binary (Binary)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression (Kind(..))
import Lamdu.Sugar.Types.Internal (T, CT, Stored, Inferred, LoadedExpr)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Sugar.Types.Internal as TypesInternal
import qualified System.Random as Random

data InputPayloadP inferred stored a
  = InputPayload
    { _ipGuid :: Guid
    , _ipInferred :: inferred
    , _ipStored :: stored
    , _ipData :: a
    }
Lens.makeLenses ''InputPayloadP

type InputPayload m a =
  InputPayloadP (Maybe (Inferred m)) (Maybe (Stored m)) a
type InputExpr m a = LoadedExpr m (InputPayload m a)

data WrapAction m
  = WrapperAlready -- I'm an apply-of-hole, no need to wrap
  | WrappedAlready Guid -- I'm an arg of apply-of-hole (Guid of apply), no need to wrap
  | WrapNotAllowed -- I'm already wrapped or a tag or a hole
  | WrapAction (T m Guid) -- Wrap me!

data Actions m = Actions
  { _storedGuid :: Guid
  , -- wrap not available for wrapped exprs or wrapper exprs
    _wrap :: WrapAction m
  , -- mSetToHole not available for holes.
    _mSetToHole :: Maybe (T m Guid)
  , _mSetToInnerExpr :: Maybe (T m Guid)
  , _cut :: T m Guid
  }

data Payload name m a = Payload
  { _plInferredTypes :: [Expression name m ()] -- TODO: Use Maybe, not []
  -- This must be embedded in the expression AST and not as a separate
  -- function so that AddNames can correct the "name" here in the
  -- right context.
  , _plActions :: Maybe (Actions m)
  , _plGuid :: Guid
  , _plData :: a
  } deriving (Functor, Foldable, Traversable)

type MStorePoint m a =
  (Maybe (TypesInternal.StorePoint (Tag m)), a)

type ExprStorePoint m a = LoadedExpr m (MStorePoint m a)

data ExpressionP name m pl = Expression
  { _rBody :: Body name m (ExpressionP name m pl)
  , _rPayload :: pl
  } deriving (Functor, Foldable, Traversable)

data NameSource = AutoGeneratedName | StoredName
  deriving (Show)
data NameCollision = NoCollision | Collision {-Disambiguator:-} Int
  deriving (Show)
data Name = Name
  { nNameSource :: NameSource
  , nNameCollisionSuffix :: NameCollision
  , nName :: String
  } deriving (Show)
type MStoredName = Maybe String

type Expression name m a = ExpressionP name m (Payload name m a)
type ExpressionN m a = Expression Name m a

type BodyN m a = Body Name m (ExpressionN m a)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m Guid
  , _itemDelete :: T m Guid
  }

data FuncParamActions name m = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  , _fpGetExample :: CT m (Expression name m ())
  }

data FuncParamType = FuncParameter | FuncFieldParameter

-- TODO:
-- FuncParam for lambda needs GetExample, but not ListItemActions
-- FuncParam for pi needs neither
-- FuncParam for definition needs both
-- So separate the types properly
data FuncParam name m expr = FuncParam
  { -- non-unique (e.g: tag guid). Name attached here:
    _fpGuid :: Guid
  , _fpId :: Guid
  , _fpAltIds :: [Guid]
  , _fpVarKind :: FuncParamType
  , _fpName :: name
  , _fpType :: expr
  , _fpInferredType :: LoadedExpr m ()
  , _fpMActions :: Maybe (FuncParamActions name m)
  } deriving (Functor, Foldable, Traversable)

data Lam name m expr = Lam
  { _lKind :: Kind
  , _lParam :: FuncParam name m expr
  , _lIsDep :: Bool
  , _lResultType :: expr
  } deriving (Functor, Foldable, Traversable)

data PickedResult = PickedResult
  { _prMJumpTo :: Maybe Guid
  , -- pairs of ids from converted expression and written expression.
    _prIdTranslation :: [(Guid, Guid)]
  }

data HoleResult name m a = HoleResult
  { _holeResultInferred :: LoadedExpr m (Inferred m)
  , _holeResultConverted :: Expression name m a
  , _holeResultPick :: T m PickedResult
  , _holeResultHasHoles :: Bool
  } deriving (Functor, Foldable, Traversable)

data HoleResultSeed m a
  = ResultSeedExpression (ExprIRef.ExpressionM m a)
  | ResultSeedNewTag String
  | ResultSeedNewDefinition String
  deriving (Functor, Foldable, Traversable)

type ScopeItem m a = (a, ExprIRef.ExpressionM m ())

data Scope name m = Scope
  { _scopeLocals    :: [ScopeItem m (GetVar name m)]
  , _scopeGlobals   :: [ScopeItem m (GetVar name m)]
  , _scopeTags      :: [ScopeItem m (TagG name)]
  , _scopeGetParams :: [ScopeItem m (GetParams name m)]
  }

data HoleActions name m = HoleActions
  { _holeScope :: T m (Scope name m)
  , -- Infer expression "on the side" (not in the hole position),
    -- but with the hole's scope.
    -- If given expression does not type check on its own, returns Nothing.
    -- (used by HoleEdit to suggest variations based on type)
    _holeInferExprType ::
      -- TODO: Just return   LoadedExpr (DerefedSTV def, a)  ?
      ExprIRef.ExpressionM m () -> CT m (Maybe (LoadedExpr m ()))
  , holeResult ::
      forall a.
      (Binary a, Typeable a, Ord a, Monoid a) =>
      (Guid -> Random.StdGen) -> -- for consistent guids
      HoleResultSeed m (Maybe (TypesInternal.StorePoint (Tag m)), a) ->
      CT m (Maybe (HoleResult name m a))
  , _holePaste :: Maybe (T m Guid)
  }

data Unwrap m
  = UnwrapMAction (Maybe (T m Guid))
  | UnwrapTypeMismatch

data HoleArg m expr = HoleArg
  { _haExpr :: expr
  , _haExprPresugared :: ExprStorePoint m ()
  , _haUnwrap :: Unwrap m
  } deriving (Functor, Foldable, Traversable)

data HoleInferred name m = HoleInferred
  { -- hiBaseValue is the inferred value WITHOUT the vars context
    -- TODO: hiBaseValue -> hiWithStructure
    _hiBaseValue :: LoadedExpr m ()
  , _hiWithVarsValue :: LoadedExpr m ()
  , _hiType :: LoadedExpr m ()
  -- The Sugar Expression of the WithVarsValue
  , _hiMakeConverted :: Random.StdGen -> CT m (Expression name m ())
  }

data Hole name m expr = Hole
  { _holeMActions :: Maybe (HoleActions name m)
  , _holeMInferred :: Maybe (HoleInferred name m)
  , _holeMArg :: Maybe (HoleArg m expr)
  } deriving (Functor, Foldable, Traversable)

data Collapsed name m expr = Collapsed
  { _cFuncGuid :: Guid
  , _cCompact :: GetVar name m
  , _cFullExpression :: expr
    -- If the full expr has info (non-hole args) we want to leave it
    -- expanded:
  , _cFullExprHasInfo :: Bool
  } deriving (Functor, Foldable, Traversable)

-- TODO: Do we want to store/allow-access to the implicit type params (nil's type, each cons type?)
data ListItem m expr = ListItem
  { _liMActions :: Maybe (ListItemActions m)
  , _liExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data ListActions m = ListActions
  { addFirstItem :: T m Guid
  , replaceNil :: T m Guid
  }

data List m expr = List
  { lValues :: [ListItem m expr]
  , lMActions :: Maybe (ListActions m)
  , -- Nil guid stays consistent when adding items.
    -- (Exposed for consistent animations)
    lNilGuid :: Guid
  } deriving (Functor, Foldable, Traversable)

data RecordField m expr = RecordField
  { _rfMItemActions :: Maybe (ListItemActions m)
  , _rfTag :: expr
  , _rfExpr :: expr -- field type or val
  } deriving (Functor, Foldable, Traversable)

data FieldList m expr = FieldList
  { _flItems :: [RecordField m expr]
  , _flMAddFirstItem :: Maybe (T m Guid)
  } deriving (Functor, Foldable, Traversable)

data Record m expr = Record
  { _rKind :: Kind -- record type or val
  , _rFields :: FieldList m expr
  } deriving (Functor, Foldable, Traversable)

data GetField expr = GetField
  { _gfRecord :: expr
  , _gfTag :: expr
  } deriving (Functor, Foldable, Traversable)

data GetVarType = GetDefinition | GetFieldParameter | GetParameter
  deriving (Eq, Ord)

data GetVar name m = GetVar
  { _gvIdentifier :: Guid
  , _gvName :: name
  , _gvJumpTo :: T m Guid
  , _gvVarType :: GetVarType
  }

data GetParams name m = GetParams
  { _gpDefGuid :: Guid
  , _gpDefName :: name
  , _gpJumpTo :: T m Guid
  }

data TagG name = TagG
  { _tagGuid :: Guid
  , _tagName :: name
  } deriving (Functor, Foldable, Traversable)

data SpecialArgs expr
  = NoSpecialArgs
  | ObjectArg expr
  | InfixArgs expr expr
  deriving (Functor, Foldable, Traversable)

data AnnotatedArg name expr = AnnotatedArg
  { _aaTag :: TagG name
  , -- Used for animation ids consistent with record.
    _aaTagExprGuid :: Guid
  , _aaExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data Apply name expr = Apply
  { _aFunc :: expr
  , _aSpecialArgs :: SpecialArgs expr
  , _aAnnotatedArgs :: [AnnotatedArg name expr]
  } deriving (Functor, Foldable, Traversable)

data Body name m expr
  = BodyLam (Lam name m expr)
  | BodyApply (Apply name expr)
  | BodyHole (Hole name m expr)
  | BodyCollapsed (Collapsed name m expr)
  | BodyLiteralInteger Integer
  | BodyAtom String
  | BodyList (List m expr)
  | BodyRecord (Record m expr)
  | BodyGetField (GetField expr)
  | BodyTag (TagG name)
  | BodyGetVar (GetVar name m)
  | BodyGetParams (GetParams name m)
  deriving (Functor, Foldable, Traversable)

instance Show expr => Show (FuncParam name m expr) where
  show fp =
    concat ["(", show (_fpGuid fp), ":", show (_fpType fp), ")"]

instance Show expr => Show (Body name m expr) where
  show (BodyLam (Lam KVal _paramType _isDep _body)) = "TODO:Lam"
  show (BodyLam (Lam KType paramType isDep resultType)) =
    paramName ++ show paramType ++ " -> " ++ show resultType
    where
      paramName | isDep = "_:"
                | otherwise = ""
  show BodyHole {} = "Hole"
  show BodyCollapsed {} = "Collapsed"
  show (BodyLiteralInteger i) = show i
  show (BodyAtom atom) = atom
  show (BodyList (List items _ _)) =
    concat
    [ "["
    , List.intercalate ", " $ map (show . _liExpr) items
    , "]"
    ]
  show BodyApply {} = "LabelledApply:TODO"
  show BodyRecord {} = "Record:TODO"
  show BodyGetField {} = "GetField:TODO"
  show BodyTag {} = "Tag:TODO"
  show BodyGetVar {} = "GetVar:TODO"
  show BodyGetParams {} = "GetParams:TODO"

data WhereItem name m expr = WhereItem
  { _wiValue :: DefinitionContent name m expr
  , _wiInferredType :: LoadedExpr m ()
  , _wiGuid :: Guid
  , _wiName :: name
  , _wiActions :: Maybe (ListItemActions m)
  } deriving (Functor, Foldable, Traversable)

-- Common data for definitions and where-items
data DefinitionContent name m expr = DefinitionContent
  { _dDepParams :: [FuncParam name m expr]
  , _dParams :: [FuncParam name m expr]
  , _dBody :: expr
  , _dWhereItems :: [WhereItem name m expr]
  , _dAddFirstParam :: T m Guid
  , _dAddInnermostWhereItem :: T m Guid
  } deriving (Functor, Foldable, Traversable)

data AcceptNewType m expr = AcceptNewType
  { antOldType :: expr
  , antNewType :: expr
  , antAccept :: T m ()
  } deriving (Functor, Foldable, Traversable)

data ShowIncompleteType expr = ShowIncompleteType
  { sitOldType :: expr
  , sitNewIncompleteType :: expr
  } deriving (Functor, Foldable, Traversable)

data DefinitionTypeInfo m expr
  = DefinitionExportedTypeInfo expr
  | DefinitionIncompleteType (ShowIncompleteType expr)
  | DefinitionNewType (AcceptNewType m expr)
  deriving (Functor, Foldable, Traversable)

data DefinitionExpression name m expr = DefinitionExpression
  { _deTypeInfo :: DefinitionTypeInfo m expr
  , _deContent :: DefinitionContent name m expr
  } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin m expr = DefinitionBuiltin
  { biName :: Definition.FFIName
  -- Consider removing Maybe'ness here
  , biMSetName :: Maybe (Definition.FFIName -> T m ())
  , biType :: expr
  } deriving (Functor, Foldable, Traversable)

data DefinitionBody name m expr
  = DefinitionBodyExpression (DefinitionExpression name m expr)
  | DefinitionBodyBuiltin (DefinitionBuiltin m expr)
  deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
  { _drGuid :: Guid
  , _drName :: name
  , _drBody :: DefinitionBody name m expr
  } deriving (Functor, Foldable, Traversable)

type DefinitionN m a = Definition Name m (Expression Name m a)
type DefinitionU m a = Definition MStoredName m (Expression MStoredName m a)

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Apply
Lens.makeLenses ''Body
Lens.makeLenses ''Collapsed
Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionContent
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''ExpressionP
Lens.makeLenses ''FieldList
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''GetField
Lens.makeLenses ''GetParams
Lens.makeLenses ''GetVar
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleActions
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleInferred
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
Lens.makePrisms ''Body
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''DefinitionTypeInfo
Lens.makePrisms ''HoleResultSeed
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''Unwrap
derive makeMonoid ''Scope
