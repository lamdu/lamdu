{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable, GeneralizedNewtypeDeriving, KindSignatures #-}
module Lamdu.Sugar.Types.Expression
    ( WrapAction(..), _WrapperAlready, _WrappedAlready, _WrapNotAllowed, _WrapAction
    , SetToHole(..), _SetToHole, _SetWrapperToHole, _AlreadyAHole
    , ExtractToDestination(..)
    , Actions(..)
        , wrap, setToHole, extract, mReplaceParent
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord
        , _BodyFromNom, _BodyToNom, _BodyGuard
    , Payload(..), plEntityId, plAnnotation, plActions, plData
    , Expression(..), rBody, rPayload
    -- record:
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , RecordTail(..), _RecordExtending, _ClosedRecord
    , RecordAddFieldResult(..), rafrNewTag, rafrNewVal, rafrRecExtend
    , Record(..), rItems, rAddField, rTail
    -- case
    , CaseTail(..), _CaseExtending, _ClosedCase
    , CaseAddAltResult(..), caarNewTag, caarNewVal, caarCase
    , CaseArg(..), caVal, caToLambdaCase
    , CaseKind(..), _LambdaCase, _CaseWithArg
    , Case(..), cKind, cAlts, cAddAlt, cTail
    , GuardElseIf(..), geScopes, geEntityId, geCond, geThen, geDelete, geCondAddLet
    , Guard(..), gIf, gThen, gElseIfs, gElse, gDeleteIf
    , Nominal(..), nTId, nVal
    --
    , GetField(..), gfRecord, gfTag
    , Inject(..), iTag, iMVal
    , ParameterForm(..), _GetFieldParameter, _GetParameter
    , NameRef(..), nrName, nrGotoDefinition
    , Param(..), pNameRef, pForm, pBinderMode
    , BinderVarForm(..), _GetDefinition, _GetLet
    , DefinitionForm(..), _DefUpToDate, _DefDeleted, _DefTypeChanged
    , DefinitionOutdatedType(..), defTypeWhenUsed, defTypeCurrent, defTypeUseCurrent
    , BinderVarInline(..), _InlineVar, _CannotInlineDueToUses, _CannotInline
    , BinderVar(..), bvNameRef, bvForm, bvInline
    , GetVar(..), _GetParam, _GetParamsRecord, _GetBinder
    , ParamsRecordVar(..), prvFieldNames
    , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
    , AnnotatedArg(..), aaTag, aaExpr, aaName
    , RelayedArg(..), raValue, raId, raActions
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Unwrap(..), _UnwrapAction, _UnwrapTypeMismatch
    , HoleArg(..), haExpr, haUnwrap
    , HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , LeafHoleActions(..), holeOptionLiteral
    , HoleActions(..), holeUUID, holeOptions, holeMDelete
    , HoleKind(..), _LeafHole, _WrapperHole
    , Hole(..), holeActions, holeKind
    , TId(..), tidName, tidTId
    , HoleResultScore
    , HoleResult(..)
        , holeResultConverted
        , holeResultPick
    , PickedResult(..), prIdTranslation
    , Lambda(..), lamBinder, lamMode
    , V.Apply(..), V.applyFunc, V.applyArg
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import qualified Data.ByteString as SBS
import           Data.Functor.Identity (Identity(..))
import           Data.Store.Transaction (Transaction, Property)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder

import           Lamdu.Prelude

type T = Transaction

data WrapAction m
    = WrapperAlready (UUID, EntityId) -- I'm an apply-of-hole, (UUID,EntityId of hole), no need to wrap
    | WrappedAlready (UUID, EntityId) -- I'm an arg of apply-of-hole (UUID,EntityId of hole), no need to wrap
    | WrapNotAllowed -- I'm a hole
    | WrapAction (T m (UUID, EntityId)) -- Wrap me!

data SetToHole m
    = SetToHole (T m (UUID, EntityId))
    | SetWrapperToHole (T m (UUID, EntityId))
    | AlreadyAHole

data ExtractToDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data Actions m = Actions
    { _wrap :: WrapAction m
    , _setToHole :: SetToHole m
    , _extract :: T m ExtractToDestination
    , _mReplaceParent :: Maybe (T m EntityId)
    }

data Payload m a = Payload
    { _plAnnotation :: Annotation
    , _plActions :: Actions m
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable)

data Expression name m a = Expression
    { _rBody :: Body name m (Expression name m a)
    , _rPayload :: Payload m a
    } deriving (Functor, Foldable, Traversable)

newtype PickedResult = PickedResult
    { _prIdTranslation :: [(EntityId, EntityId)]
    }

type HoleResultScore = [Int]

data HoleResult name m = HoleResult
    { _holeResultConverted :: Expression name m ()
    , _holeResultPick :: T m PickedResult
    }

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    }

data HoleOption name m = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: T m (Expression name m ())
    , -- A group in the hole results based on this option
      _hoResults :: ListT (T m) (HoleResultScore, T m (HoleResult name m))
    }

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f SBS.ByteString)
    | LiteralText (f Text)

data HoleActions name m = HoleActions
    { _holeUUID :: UUID -- TODO: Replace this with a way to associate data?
    , _holeOptions :: T m [HoleOption name m]
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (T m EntityId)
    }

newtype LeafHoleActions name m = LeafHoleActions
    { _holeOptionLiteral :: Literal Identity -> T m (HoleOption name m)
    }

data Unwrap m
    = UnwrapAction (T m EntityId)
    | UnwrapTypeMismatch

data HoleArg m expr = HoleArg
    { _haExpr :: expr
    , _haUnwrap :: Unwrap m
    } deriving (Functor, Foldable, Traversable)

data HoleKind name m expr
    = LeafHole (LeafHoleActions name m)
    | WrapperHole (HoleArg m expr)
    deriving (Functor, Foldable, Traversable)

data Hole name m expr = Hole
    { _holeActions :: HoleActions name m
    , _holeKind :: HoleKind name m expr
    } deriving (Functor, Foldable, Traversable)

{- Record start -}
data CompositeItem name m expr = CompositeItem
    { _ciDelete :: T m EntityId
    , _ciTag :: Tag name m
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable)

data RecordTail m expr
    = RecordExtending expr
    | ClosedRecord (T m EntityId) -- delete action
    deriving (Functor, Foldable, Traversable)

data RecordAddFieldResult = RecordAddFieldResult
    { _rafrNewTag :: TagInfo
    , _rafrNewVal :: EntityId
    , _rafrRecExtend :: EntityId
    }

data Record name m expr = Record
    { _rItems :: [CompositeItem name m expr]
    , _rTail :: RecordTail m expr
    , _rAddField :: T m RecordAddFieldResult
    } deriving (Functor, Foldable, Traversable)
{- Record end -}

data CaseTail m expr
    = CaseExtending expr
    | ClosedCase (T m EntityId) -- delete action
    deriving (Functor, Foldable, Traversable)

data CaseAddAltResult = CaseAddAltResult
    { _caarNewTag :: TagInfo
    , _caarNewVal :: EntityId
    , _caarCase :: EntityId
    }

data CaseArg m expr = CaseArg
    { _caVal :: expr
    , _caToLambdaCase :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data CaseKind m expr
    = LambdaCase
    | CaseWithArg (CaseArg m expr)
    deriving (Functor, Foldable, Traversable)

data Case name m expr = Case
    { _cKind :: CaseKind m expr
    , _cAlts :: [CompositeItem name m expr]
    , _cTail :: CaseTail m expr
    , _cAddAlt :: T m CaseAddAltResult
    } deriving (Functor, Foldable, Traversable)
{- Case end -}

data GuardElseIf m expr = GuardElseIf
    { _geScopes :: ChildScopeMapping
    , _geEntityId :: EntityId
    , _geCond :: expr
    , _geThen :: expr
    , _geDelete :: T m EntityId
    , _geCondAddLet :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data Guard m expr = Guard
    { -- "if" is different than "else if" in that it doesn't have a scope that it run in
      _gIf :: expr
    , _gThen :: expr
    , _gElseIfs :: [GuardElseIf m expr]
    , _gElse :: expr
    , _gDeleteIf :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data GetField name m expr = GetField
    { _gfRecord :: expr
    , _gfTag :: Tag name m
    } deriving (Functor, Foldable, Traversable)

data Inject name m expr = Inject
    { _iTag :: Tag name m
    , _iMVal :: Maybe expr
    } deriving (Functor, Foldable, Traversable)

data ParameterForm = GetFieldParameter | GetParameter
    deriving (Eq, Ord)

data NameRef name m = NameRef
    { _nrName :: name
    , _nrGotoDefinition :: T m EntityId
    }

data Param name m = Param
    { _pNameRef :: NameRef name m
    , _pForm :: ParameterForm
    , _pBinderMode :: BinderMode
    }

data DefinitionOutdatedType m = DefinitionOutdatedType
    { _defTypeWhenUsed :: Scheme
    , _defTypeCurrent :: Scheme
    , _defTypeUseCurrent :: T m ()
    }

data DefinitionForm m =
    DefUpToDate | DefDeleted | DefTypeChanged (DefinitionOutdatedType m)

data BinderVarForm m = GetDefinition (DefinitionForm m) | GetLet

data BinderVarInline m
    = InlineVar (T m EntityId)
    | CannotInlineDueToUses [EntityId]
    | CannotInline

data BinderVar name m = BinderVar
    { _bvNameRef :: NameRef name m
    , _bvForm :: BinderVarForm m
    , -- Just means it is stored and inlinable:
      _bvInline :: BinderVarInline m
    }

newtype ParamsRecordVar name = ParamsRecordVar
    { _prvFieldNames :: [name]
    } deriving (Eq, Ord, Functor, Foldable, Traversable)

data GetVar name m
    = GetParam (Param name m)
    | GetParamsRecord (ParamsRecordVar name)
    | GetBinder (BinderVar name m)

data SpecialArgs expr
    = NoSpecialArgs
    | ObjectArg expr
    | InfixArgs expr expr
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo
    , _aaName :: name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable)

data RelayedArg name m = RelayedArg
    { _raValue :: Param name m
    , _raId :: EntityId
    , _raActions :: Actions m
    }

data LabeledApply name m expr = LabeledApply
    { _aFunc :: BinderVar name m
    , _aSpecialArgs :: SpecialArgs expr
    , _aAnnotatedArgs :: [AnnotatedArg name expr]
    , _aRelayedArgs :: [RelayedArg name m]
    } deriving (Functor, Foldable, Traversable)

data Nominal name expr = Nominal
    { _nTId :: TId name
    , _nVal :: expr
    } deriving (Functor, Foldable, Traversable)

data Lambda name m expr = Lambda
    { _lamMode :: BinderMode
    , _lamBinder :: Binder name m expr
    } deriving (Functor, Foldable, Traversable)

data Body name m expr
    = BodyLam (Lambda name m expr)
    | BodySimpleApply (V.Apply expr)
    | BodyLabeledApply (LabeledApply name m expr)
    | BodyHole (Hole name m expr)
    | BodyLiteral (Literal (Property m))
    | BodyRecord (Record name m expr)
    | BodyGetField (GetField name m expr)
    | BodyCase (Case name m expr)
    | BodyGuard (Guard m expr)
    | BodyInject (Inject name m expr)
    | BodyGetVar (GetVar name m)
    | BodyToNom (Nominal name (BinderBody name m expr))
    | BodyFromNom (Nominal name expr)
    | BodyInjectedExpression -- Used for hole results
    deriving (Functor, Foldable, Traversable)

instance Show (Body name m expr) where
    show (BodyLam _) = "TODO show lam"
    show BodyHole {} = "Hole"
    show BodyLiteral {} = "Literal"
    show BodySimpleApply {} = "SimpleApply:TODO"
    show BodyLabeledApply {} = "LabelledApply:TODO"
    show BodyRecord {} = "Record:TODO"
    show BodyGetField {} = "GetField:TODO"
    show BodyCase {} = "Case:TODO"
    show BodyGuard {} = "If:TODO"
    show BodyInject {} = "Inject:TODO"
    show BodyGetVar {} = "GetVar:TODO"
    show BodyFromNom {} = "FromNom:TODO"
    show BodyToNom {} = "ToNom:TODO"
    show BodyInjectedExpression {} = "InjectedExpression"

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''BinderVar
Lens.makeLenses ''Body
Lens.makeLenses ''Case
Lens.makeLenses ''CaseAddAltResult
Lens.makeLenses ''CaseArg
Lens.makeLenses ''CompositeItem
Lens.makeLenses ''DefinitionOutdatedType
Lens.makeLenses ''Expression
Lens.makeLenses ''GetField
Lens.makeLenses ''Guard
Lens.makeLenses ''GuardElseIf
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleActions
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''LeafHoleActions
Lens.makeLenses ''NameRef
Lens.makeLenses ''Nominal
Lens.makeLenses ''Param
Lens.makeLenses ''ParamsRecordVar
Lens.makeLenses ''Payload
Lens.makeLenses ''PickedResult
Lens.makeLenses ''Record
Lens.makeLenses ''RecordAddFieldResult
Lens.makeLenses ''RelayedArg
Lens.makeLenses ''TId
Lens.makePrisms ''BinderVarForm
Lens.makePrisms ''BinderVarInline
Lens.makePrisms ''Body
Lens.makePrisms ''CaseKind
Lens.makePrisms ''CaseTail
Lens.makePrisms ''DefinitionForm
Lens.makePrisms ''GetVar
Lens.makePrisms ''HoleKind
Lens.makePrisms ''Literal
Lens.makePrisms ''ParameterForm
Lens.makePrisms ''RecordTail
Lens.makePrisms ''SetToHole
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''Unwrap
Lens.makePrisms ''WrapAction
