{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards #-}
module Lamdu.Sugar.Types
    ( EntityId
    , Definition(..), drEntityId, drName, drBody
    , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
    , ListItemActions(..), itemAddNext, itemDelete
    , VarToTags(..), TagsToVar(..)
    , ParamDelResult(..), ParamAddResult(..)
    , FuncParamActions(..), fpAddNext, fpDelete
    , DefinitionExpression(..), deContent, deTypeInfo
    , AcceptNewType(..)
    , DefinitionTypeInfo(..)
        , _DefinitionExportedTypeInfo
        , _DefinitionNewType
    , Anchors.PresentationMode(..)
    , BinderActions(..)
        , baAddFirstParam, baAddInnermostWhereItem
    , BinderParams(..), _NoParams, _VarParam, _FieldParams
    , Binder(..)
        , dSetPresentationMode, dParams, dBody, dWhereItems, dMActions
    , DefinitionBuiltin(..), biType, biName, biSetName
    , WrapAction(..), _WrapperAlready, _WrappedAlready, _WrapNotAllowed, _WrapAction
    , SetToHole(..), _SetToHole, _AlreadyAHole
    , SetToInnerExpr(..), _SetToInnerExpr, _NoInnerExpr
    , Actions(..)
        , wrap, setToHole, setToInnerExpr, cut
    , Body(..)
        , _BodyLam, _BodyApply, _BodyGetVar, _BodyGetField, _BodyHole
        , _BodyLiteralInteger, _BodyList, _BodyRecord
    , EvaluationResult
    , Annotation(..), aInferredType, aMEvaluationResult
    , Payload(..), plEntityId, plAnnotation, plActions, plData
    , Expression(..), rBody, rPayload
    , DefinitionU
    , WhereItem(..), wiEntityId, wiValue, wiName, wiActions, wiAnnotation
    , ListItem(..), liMActions, liExpr
    , ListActions(..), List(..)
    , RecordField(..), rfMDelete, rfTag, rfExpr
    , RecordTail(..), _RecordExtending, _ClosedRecord
    , RecordAddFieldResult(..), rafrNewTag, rafrNewVal, rafrRecExtend
    , Record(..), rItems, rMAddField, rTail
    , GetField(..), gfRecord, gfTag
    , NamedVarType(..)
    , NamedVar(..), nvName, nvJumpTo, nvVarType
    , GetVar(..), _GetVarNamed, _GetVarParamsRecord
    , ParamsRecordVar(..), prvFieldNames
    , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
    , AnnotatedArg(..), aaTag, aaExpr
    , Apply(..), aFunc, aSpecialArgs, aAnnotatedArgs
    , FuncParam(..)
        , fpName, fpId, fpVarInfo, fpAnnotation, fpMActions, fpHiddenIds
    , Unwrap(..), _UnwrapMAction, _UnwrapTypeMismatch
    , HoleArg(..), haExpr, haUnwrap
    , Hole(..)
        , holeMActions, holeMArg, holeSuggested, holeGuid
    , ScopeGetVar(..), sgvGetVar, sgvVal
    , HoleActions(..)
        , holeScope, holePaste, holeResults
    , HoleResultScore
    , HoleResult(..)
        , holeResultConverted
        , holeResultPick
    , IsInjected(..)
    , PickedResult(..), prIdTranslation
    , TagG(..), tagGName, tagVal, tagInstance
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Foldable (Foldable)
import qualified Data.List as List
import           Data.Monoid (Monoid(..))
import           Data.Store.Guid (Guid)
import           Data.Store.Transaction (Transaction, MkProperty)
import           Data.Traversable (Traversable)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Scheme (Scheme)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)

type T = Transaction

data WrapAction m
    = WrapperAlready (Guid, EntityId) -- I'm an apply-of-hole, (Guid,EntityId of hole), no need to wrap
    | WrappedAlready (Guid, EntityId) -- I'm an arg of apply-of-hole (Guid,EntityId of hole), no need to wrap
    | WrapNotAllowed -- I'm a hole
    | WrapAction (T m (Guid, EntityId)) -- Wrap me!

data SetToHole m
    = SetToHole (T m (Guid, EntityId))
    | AlreadyAHole

data SetToInnerExpr m = SetToInnerExpr (T m EntityId) | NoInnerExpr

data Actions m = Actions
    { _wrap :: WrapAction m
    , _setToHole :: SetToHole m
    , _setToInnerExpr :: SetToInnerExpr m
    , _cut :: Maybe (T m EntityId) -- Nothing if already hole
    }

type EvaluationResult = String

data Annotation = Annotation
    { _aInferredType :: Type
    , _aMEvaluationResult :: Maybe EvaluationResult
    } deriving (Show)

data Payload m a = Payload
    { _plAnnotation :: Annotation
    , _plActions :: Maybe (Actions m)
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable)

data Expression name m a = Expression
    { _rBody :: Body name m (Expression name m a)
    , _rPayload :: Payload m a
    } deriving (Functor, Foldable, Traversable)

data ListItemActions m = ListItemActions
    { _itemAddNext :: T m EntityId
    , _itemDelete :: T m ()
    }

data VarToTags = VarToTags
    { vttReplacedVar :: V.Var
    , vttReplacedVarEntityId :: EntityId
      -- Since this is just a result of a transaction, no name is
      -- actually needed in the Tags below
    , vttReplacedByTag :: TagG ()
    , vttNewTag :: TagG ()
    }

data ParamAddResult
    = ParamAddResultNewVar EntityId V.Var
    | ParamAddResultVarToTags VarToTags
    | ParamAddResultNewTag (TagG ())

data TagsToVar = TagsToVar
    { ttvReplacedTag :: TagG ()
    , ttvReplacedByVar :: V.Var
    , ttvReplacedByVarEntityId :: EntityId
    , ttvDeletedTag :: TagG ()
    }

data ParamDelResult
    = ParamDelResultDelVar
    | ParamDelResultTagsToVar TagsToVar
    | ParamDelResultDelTag

data FuncParamActions m = FuncParamActions
    { _fpAddNext :: T m ParamAddResult
    , _fpDelete :: T m ParamDelResult
    }

data FuncParam varinfo name m = FuncParam
    { _fpId :: EntityId
    , _fpVarInfo :: varinfo
    , _fpName :: name
    , _fpAnnotation :: Annotation
    , _fpMActions :: Maybe (FuncParamActions m)
    , -- Sometimes the Lambda disappears in Sugar, the Param "swallows" its id
        _fpHiddenIds :: [EntityId]
    }

data TagG name = TagG
    { _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    , _tagGName :: name
    }

newtype PickedResult = PickedResult
    { _prIdTranslation :: [(EntityId, EntityId)]
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
    }

data ScopeGetVar name m = ScopeGetVar
    { _sgvGetVar :: GetVar name m
    , _sgvVal :: Val ()
    }

data HoleActions name m = HoleActions
    { _holeScope :: T m [ScopeGetVar name m]
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

data Hole name m expr = Hole
    { _holeMActions :: Maybe (HoleActions name m)
    , _holeSuggested :: Val ()
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

data RecordTail m expr
    = RecordExtending expr
    | ClosedRecord (Maybe (T m EntityId)) -- delete action
    deriving (Functor, Foldable, Traversable)

data RecordAddFieldResult = RecordAddFieldResult
    { _rafrNewTag :: TagG ()
    , _rafrNewVal :: EntityId
    , _rafrRecExtend :: EntityId
    }

data Record name m expr = Record
    { _rItems :: [RecordField name m expr]
    , _rTail :: RecordTail m expr
    , _rMAddField :: Maybe (T m RecordAddFieldResult)
    } deriving (Functor, Foldable, Traversable)

data GetField name expr = GetField
    { _gfRecord :: expr
    , _gfTag :: TagG name
    } deriving (Functor, Foldable, Traversable)

data NamedVarType = GetDefinition | GetFieldParameter | GetParameter
    deriving (Eq, Ord)

data NamedVar name m = NamedVar
    { _nvName :: name
    , _nvJumpTo :: T m EntityId
    , _nvVarType :: NamedVarType
    }

newtype ParamsRecordVar name = ParamsRecordVar
    { _prvFieldNames :: [name]
    } deriving (Eq, Ord, Functor, Foldable, Traversable)

data GetVar name m
    = GetVarNamed (NamedVar name m)
    | GetVarParamsRecord (ParamsRecordVar name)

data SpecialArgs expr
    = NoSpecialArgs
    | ObjectArg expr
    | InfixArgs expr expr
    deriving (Functor, Foldable, Traversable)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagG name
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

instance (Show paraminfo, Show name) => Show (FuncParam paraminfo name m) where
    show FuncParam{..} = "(FuncParam " ++ show _fpId ++ " " ++ show _fpVarInfo ++ " " ++ show _fpName ++
                                              " " ++ show _fpAnnotation ++ " )"


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
    , _wiAnnotation :: Annotation
    , _wiName :: name
    , _wiActions :: Maybe (ListItemActions m)
    } deriving (Functor, Foldable, Traversable)

data BinderActions m = BinderActions
    { _baAddFirstParam :: T m ParamAddResult
    , _baAddInnermostWhereItem :: T m EntityId
    }

data BinderParams name m
    = NoParams -- used in definitions and where items
    | VarParam (FuncParam () name m)
    | FieldParams [FuncParam T.Tag name m]

data Binder name m expr = Binder
    { _dSetPresentationMode :: Maybe (MkProperty m Anchors.PresentationMode)
    , _dParams :: BinderParams name m
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
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> T m ()
    , _biType :: Scheme
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
Lens.makeLenses ''Annotation
Lens.makeLenses ''Apply
Lens.makeLenses ''Binder
Lens.makeLenses ''BinderActions
Lens.makeLenses ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''Expression
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''GetField
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleActions
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleResult
Lens.makeLenses ''ListItem
Lens.makeLenses ''ListItemActions
Lens.makeLenses ''NamedVar
Lens.makeLenses ''ParamsRecordVar
Lens.makeLenses ''Payload
Lens.makeLenses ''PickedResult
Lens.makeLenses ''Record
Lens.makeLenses ''RecordField
Lens.makeLenses ''RecordAddFieldResult
Lens.makeLenses ''ScopeGetVar
Lens.makeLenses ''TagG
Lens.makeLenses ''WhereItem
Lens.makePrisms ''Body
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''DefinitionTypeInfo
Lens.makePrisms ''GetVar
Lens.makePrisms ''RecordTail
Lens.makePrisms ''SetToHole
Lens.makePrisms ''SetToInnerExpr
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''Unwrap
Lens.makePrisms ''WrapAction
Lens.makePrisms ''BinderParams
