-- | Different leaf types in the Sugar expressions.
-- These don't contain more expressions in them.
{-# LANGUAGE TemplateHaskell, DataKinds, GADTs, TypeFamilies, MultiParamTypeClasses #-}
module Lamdu.Sugar.Types.Parts
    ( FuncApplyLimit(..), _UnlimitedFuncApply, _AtMostOneFuncApply
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralChar, _LiteralText
    , -- Annotations
      Annotation(..), _AnnotationVal, _AnnotationType, _AnnotationNone
    -- Node actions
    , DetachAction(..), _FragmentedAlready, _DetachAction
    , Delete(..), _SetToHole, _Delete, _CannotDelete
    , NodeActions(..), detach, delete, setToLiteral, extract, mReplaceParent, mApply
    , -- Let
      ExtractDestination(..)
    , -- Expressions
      Payload(..), plEntityId, plAnnotation, plActions, plHiddenEntityIds, plParenInfo
    , ClosedCompositeActions(..), closedCompositeOpen
    , PunnedVar(..), pvVar, pvTagEntityId
    , NullaryInject(..), iInject, iContent
    , Option(..), optionExpr, optionPick, optionTypeMatch, optionMNewTag
    , Query(..), qLangInfo, qTagSuffixes, qSearchTerm
    , QueryLangInfo(..), qLangId, qLangDir, qCodeTexts, qUITexts, qNameTexts
        , hasQueryLangInfo
    , TaggedVarId(..), TagSuffixes
    , Expr
    , ParenInfo(..), piNeedParens, piMinOpPrec
    , TypeMismatch(..), tmType, tmReason
    , TypeMismatchReason(..), _TypesCannotUnify, _TypeVarSkolemEscape
    ) where

import qualified Control.Lens as Lens
import           Data.Kind (Type)
import           Data.UUID.Types (UUID)
import           GUI.Momentu.Direction (Layout)
import           Hyper (makeHTraversableAndBases, makeHMorph)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.GetVar (GetVar)
import           Lamdu.Sugar.Types.Tag
import qualified Lamdu.Sugar.Types.Type as SugarType

import           Lamdu.Prelude

type Expr e v name (i :: Type -> Type) o = Annotated (Payload v o) # e v name i o

-- Can a lambda be called more than once?
-- Used to avoid showing scope selector presentations for case alternative lambdas.
data FuncApplyLimit = UnlimitedFuncApply | AtMostOneFuncApply
    deriving (Eq, Ord, Generic)

data Annotation v name
    = AnnotationType (Annotated EntityId # SugarType.Type name)
    | AnnotationVal v
    | AnnotationNone
    deriving Generic

data ExtractDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data DetachAction o
    = FragmentedAlready EntityId -- Already a fragment, its expr or hole
    | DetachAction (o EntityId) -- Detach me
    deriving Generic

data Delete m
    = SetToHole (m EntityId)
    | -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      Delete (m EntityId)
    | CannotDelete
    deriving Generic

data NodeActions o = NodeActions
    { _detach :: DetachAction o
    , _delete :: Delete o
    , _setToLiteral :: Literal Identity -> o EntityId
    , _extract :: o EntityId
    , _mReplaceParent :: Maybe (o EntityId)
    , _mApply :: Maybe (o EntityId)
    } deriving Generic

-- VarInfo is used for:
-- * Differentiating Mut actions so UI can suggest executing them
-- * Name pass giving parameters names according to types
data VarInfo
    = VarNominal (SugarType.TId T.Tag)
    | VarGeneric | VarFunction | VarRecord | VarUnit | VarVariant | VarVoid
    deriving (Generic, Eq)

data Payload v o = Payload
    { _plAnnotation :: v
    , _plActions :: NodeActions o
    , _plEntityId :: EntityId
    , _plParenInfo :: !ParenInfo
    , _plHiddenEntityIds :: [EntityId]
    } deriving Generic

newtype ClosedCompositeActions o = ClosedCompositeActions
    { _closedCompositeOpen :: o EntityId
    } deriving stock Generic

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralChar (f Char)
    | LiteralText (f Text)
    deriving Generic

data NullaryInject name i o k = NullaryInject
    { _iInject :: k :# Const (TagRef name i o)
    , -- Child represents the empty record, and has action to add an item
      _iContent :: k :# Const (i (TagChoice name o))
    } deriving Generic

data PunnedVar name o k = PunnedVar
    { _pvVar :: k :# Const (GetVar name o)
    , _pvTagEntityId :: EntityId
    } deriving Generic

data ParenInfo = ParenInfo
    { _piMinOpPrec :: !Int
    , _piNeedParens :: !Bool
    } deriving (Eq, Show, Generic)

data Option t name i o = Option
    { _optionExpr :: Expr t (Annotation () name) name i o
    , _optionPick :: o ()
    , -- Whether option expr fits the destination or will it be fragmented?
      -- Note that for fragments, this doesn't indicate whether the emplaced fragmented expr
      -- within stays fragmented.
      _optionTypeMatch :: Bool
    , -- Whether option includes creation of new tag for the given search string
      _optionMNewTag :: Maybe T.Tag
    } deriving Generic

data QueryLangInfo = QueryLangInfo
    { _qLangId :: LangId
    , _qLangDir :: Layout
    , _qCodeTexts :: Texts.Code Text
    , _qUITexts :: Texts.CodeUI Text
    , _qNameTexts :: Texts.Name Text
    }

hasQueryLangInfo :: _ => a -> QueryLangInfo
hasQueryLangInfo env = QueryLangInfo (env ^. has) (env ^. has) (env ^. has) (env ^. has) (env ^. has)

-- Like InternalName, but necessarily with a context and non-anonymous tag
data TaggedVarId = TaggedVarId
    { _tvCtx :: UUID -- InternalName's context
    , _tvTag :: T.Tag -- InternalName's tag
    } deriving (Eq, Ord, Show, Generic)

type TagSuffixes = Map TaggedVarId Int

data Query = Query
    { _qLangInfo :: QueryLangInfo
    , _qTagSuffixes :: TagSuffixes
    , _qSearchTerm :: Text
    }

data TypeMismatchReason name
    = TypesCannotUnify
    | TypesCannotUnifyDueToConstraints
    | TypeVarSkolemEscape (Annotated EntityId # SugarType.Type name)
    | TypeVarOccursInItself (Annotated EntityId # SugarType.Type name)
    deriving Generic

data TypeMismatch name = TypeMismatch
    { _tmType :: Annotated EntityId # SugarType.Type name -- Type of fragmented (mismatching) expression
    , _tmReason :: TypeMismatchReason name
    } deriving Generic

traverse Lens.makeLenses
    [ ''ClosedCompositeActions, ''NodeActions
    , ''NullaryInject, ''Option, ''ParenInfo, ''Payload, ''PunnedVar
    , ''Query, ''QueryLangInfo, ''TypeMismatch
    ] <&> concat
traverse Lens.makePrisms
    [ ''Annotation, ''Delete, ''DetachAction
    , ''FuncApplyLimit, ''Literal, ''VarInfo, ''TypeMismatchReason
    ] <&> concat
traverse makeHTraversableAndBases [''NullaryInject, ''PunnedVar] <&> concat
makeHMorph ''NullaryInject
