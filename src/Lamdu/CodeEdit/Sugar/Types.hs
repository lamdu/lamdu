{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, KindSignatures, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.CodeEdit.Sugar.Types
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
    , giveAsArg, callWithArg, callWithNextArg
    , setToHole, replaceWithNewHole, cut, giveAsArgToOperator
  , ExpressionBody(..), eHasParens
    , _ExpressionPi, _ExpressionApply, _ExpressionSection
    , _ExpressionFunc, _ExpressionGetVariable, _ExpressionHole
    , _ExpressionInferred, _ExpressionPolymorphic
    , _ExpressionLiteralInteger, _ExpressionAtom
    , _ExpressionList
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..)
    , rGuid, rExpressionBody, rPayload, rHiddenGuids, rPresugaredExpression
  , Expression
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), RecordKind(..), Record(..)
  , Func(..), fDepParams, fParams, fBody
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), holeScope, holeMActions
  , HoleActions(..)
    , holePaste, holeMDelete, holeResult, holeInferExprType
  , StorePoint(..)
  , HoleResult(..)
    , holeResultInferred
    , holeResultConvert
    , holeResultPick, holeResultPickPrefix
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  , T, CT
  , PrefixAction, emptyPrefixAction
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Foldable (Foldable)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression (RecordKind(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens.TH as LensTH
import qualified Data.List as List
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer

type T = Transaction
type CT m = StateT Cache (T m)

type PrefixAction m = T m ()

emptyPrefixAction :: Monad m => PrefixAction m
emptyPrefixAction = return ()

data Actions m = Actions
  { _giveAsArg :: PrefixAction m -> T m Guid
  -- Turn "x" to "x ? _" where "?" is an operator-hole.
  -- Given string is initial hole search term.
  , _giveAsArgToOperator :: T m Guid
  , _callWithNextArg :: PrefixAction m -> CT m (Maybe (T m Guid))
  , _callWithArg :: PrefixAction m -> CT m (Maybe (T m Guid))
  , _setToHole :: T m Guid
  , _replaceWithNewHole :: T m Guid
  , _cut :: T m Guid
  }
LensTH.makeLenses ''Actions

data HasParens = HaveParens | DontHaveParens

data Payload m = Payload
  { _plInferredTypes :: [Expression m]
  , _plActions :: Maybe (Actions m)
  , _plNextHole :: Maybe (Expression m)
  }

newtype StorePoint t = StorePoint { unStorePoint :: DataIRef.ExpressionI t }
  deriving (Eq, Binary, Typeable)

data ExpressionP m pl = Expression
  { _rGuid :: Guid
  , _rExpressionBody :: ExpressionBody m (ExpressionP m pl)
  , _rPayload :: pl
  , -- Guids from data model expression which were sugared out into
    -- this sugar expression.
    -- If the cursor was on them for whatever reason, it should be
    -- mapped into the sugar expression's guid.
    _rHiddenGuids :: [Guid]
  , _rPresugaredExpression :: DataIRef.ExpressionM m (Maybe (StorePoint (Tag m)))
  } deriving (Functor, Foldable, Traversable)

type Expression m = ExpressionP m (Payload m)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m Guid
  , _itemDelete :: T m Guid
  }

data FuncParamActions m = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  , _fpGetExample :: CT m (Expression m)
  }

data FuncParam m expr = FuncParam
  { _fpGuid :: Guid
  , _fpHiddenLambdaGuid :: Maybe Guid
  , _fpType :: expr
  , _fpMActions :: Maybe (FuncParamActions m)
  } deriving (Functor, Foldable, Traversable)

-- Multi-param Lambda
data Func m expr = Func
  { _fDepParams :: [FuncParam m expr]
  , _fParams :: [FuncParam m expr]
  , _fBody :: expr
  } deriving (Functor, Foldable, Traversable)

data Pi m expr = Pi
  { pParam :: FuncParam m expr
  , pResultType :: expr
  } deriving (Functor, Foldable, Traversable)

-- Infix Sections include: (+), (1+), (+1), (1+2). Last is really just
-- infix application, but considered an infix section too.
data Section expr = Section
  { sectionLArg :: Maybe expr
  , sectionOp :: expr -- TODO: Always a Data.GetVariable, use a more specific type
  , sectionRArg :: Maybe expr
  } deriving (Functor, Foldable, Traversable)

data HoleResult m = HoleResult
  { _holeResultInferred :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))
  , _holeResultConvert :: T m (Expression m)
  , _holeResultPick :: T m Guid
  , _holeResultPickPrefix :: PrefixAction m
  }

data HoleActions m = HoleActions
  { _holeScope :: [Guid]
  , -- Infer expression "on the side" (not in the hole position),
    -- but with the hole's scope in scope.
    -- If given expression does not type check on its own, returns Nothing.
    -- (used by HoleEdit to suggest variations based on type)
    _holeInferExprType :: DataIRef.ExpressionM m () -> CT m (Maybe (DataIRef.ExpressionM m ()))
  , _holeResult ::
      DataIRef.ExpressionM m (Maybe (StorePoint (Tag m))) ->
      CT m (Maybe (HoleResult m))
  , _holePaste :: Maybe (T m Guid)
  , -- TODO: holeMDelete is always Nothing, not implemented yet
    _holeMDelete :: Maybe (T m Guid)
  }

newtype Hole m = Hole
  { _holeMActions :: Maybe (HoleActions m)
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred m expr = Inferred
  { iValue :: expr
  , iHole :: Hole m
  } deriving (Functor, Foldable, Traversable)

data Polymorphic t expr = Polymorphic
  { pFuncGuid :: Guid
  , pCompact :: Expression.VariableRef (DefI t)
  , pFullExpression :: expr
  } deriving (Functor, Foldable, Traversable)

-- TODO: Do we want to store/allow-access to the implicit type params (nil's type, each cons type?)
data ListItem m expr = ListItem
  { liMActions :: Maybe (ListItemActions m)
  , liExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data ListActions m = ListActions
  { addFirstItem :: T m Guid
  , replaceNil :: T m Guid
  }

data List m expr = List
  { lValues :: [ListItem m expr]
  , lMActions :: Maybe (ListActions m)
  } deriving (Functor, Foldable, Traversable)

data RecordField m expr = RecordField
  { rfMDel :: Maybe (T m Guid)
  , rfId :: Expression.Field
  , rfExpr :: expr -- field type or val
  } deriving (Functor, Foldable, Traversable)

data Record m expr = Record
  { rKind :: RecordKind -- record type or val
  , rFields :: [RecordField m expr]
  , rMAddField :: Maybe (T m Guid)
  } deriving (Functor, Foldable, Traversable)

data ExpressionBody m expr
  = ExpressionApply   { _eHasParens :: HasParens, __eApply :: Expression.Apply expr }
  | ExpressionSection { _eHasParens :: HasParens, __eSection :: Section expr }
  | ExpressionFunc    { _eHasParens :: HasParens, __eFunc :: Func m expr }
  | ExpressionPi      { _eHasParens :: HasParens, __ePi :: Pi m expr }
  | ExpressionGetVariable { __getVariable :: Expression.VariableRef (DefI (Tag m)) }
  | ExpressionHole    { __eHole :: Hole m }
  | ExpressionInferred { __eInferred :: Inferred m expr }
  | ExpressionPolymorphic { __ePolymorphic :: Polymorphic (Tag m) expr }
  | ExpressionLiteralInteger { __eLit :: LiteralInteger m }
  | ExpressionAtom    { __eAtom :: String }
  | ExpressionList    { __eList :: List m expr }
  | ExpressionRecord  { __eRecord :: Record m expr }
  deriving (Functor, Foldable, Traversable)
LensTH.makePrisms ''ExpressionBody

wrapParens :: HasParens -> String -> String
wrapParens HaveParens x = concat ["(", x, ")"]
wrapParens DontHaveParens x = x

instance Show expr => Show (FuncParam m expr) where
  show fp =
    concat ["(", show (_fpGuid fp), ":", show (_fpType fp), ")"]

instance Show expr => Show (ExpressionBody m expr) where
  show ExpressionApply   { _eHasParens = hasParens, __eApply = Expression.Apply func arg } =
    wrapParens hasParens $ show func ++ " " ++ show arg
  show ExpressionSection { _eHasParens = hasParens, __eSection = Section mleft op mright } =
    wrapParens hasParens $ maybe "" show mleft ++ " " ++ show op ++ maybe "" show mright
  show ExpressionFunc    { _eHasParens = hasParens, __eFunc = Func depParams params body } =
    wrapParens hasParens $ concat
    ["\\", parenify (showWords depParams), showWords params, " -> ", show body]
    where
      parenify "" = ""
      parenify xs = concat ["{", xs, "}"]
      showWords = unwords . map show
  show ExpressionPi      { _eHasParens = hasParens, __ePi = Pi param resultType } =
    wrapParens hasParens $ "_:" ++ show param ++ " -> " ++ show resultType
  show ExpressionGetVariable { __getVariable = Expression.ParameterRef guid } = 'P' : show guid
  show ExpressionGetVariable { __getVariable = Expression.DefinitionRef defI } = 'D' : show (IRef.guid defI)
  show ExpressionHole {} = "Hole"
  show ExpressionInferred {} = "Inferred"
  show ExpressionPolymorphic {} = "Poly"
  show ExpressionLiteralInteger { __eLit = LiteralInteger i _ } = show i
  show ExpressionAtom { __eAtom = atom } = atom
  show ExpressionList { __eList = List items _ } =
    concat
    [ "["
    , List.intercalate ", " $ map (show . liExpr) items
    , "]"
    ]
  show ExpressionRecord { __eRecord = _ } = "Record:TODO"

data DefinitionNewType m = DefinitionNewType
  { dntNewType :: Expression m
  , dntAcceptNewType :: T m ()
  }

data WhereItem m = WhereItem
  { wiValue :: DefinitionContent m
  , wiGuid :: Guid
  , wiHiddenGuids :: [Guid]
  , wiActions :: Maybe (ListItemActions m)
  }

-- Common data for definitions and where-items
data DefinitionContent m = DefinitionContent
  { dFunc :: Func m (Expression m)
  , dWhereItems :: [WhereItem m]
  , dAddFirstParam :: T m Guid
  , dAddInnermostWhereItem :: T m Guid
  }

data DefinitionExpression m = DefinitionExpression
  { deContent :: DefinitionContent m
  , deIsTypeRedundant :: Bool
  , deMNewType :: Maybe (DefinitionNewType m)
  }

data DefinitionBuiltin m = DefinitionBuiltin
  { biName :: Definition.FFIName
  -- Consider removing Maybe'ness here
  , biMSetName :: Maybe (Definition.FFIName -> T m ())
  }

data DefinitionBody m
  = DefinitionBodyExpression (DefinitionExpression m)
  | DefinitionBodyBuiltin (DefinitionBuiltin m)

data Definition m = Definition
  { drGuid :: Guid
  , drType :: Expression m
  , drBody :: DefinitionBody m
  }

LensTH.makeLenses ''Func
LensTH.makeLenses ''FuncParam
LensTH.makeLenses ''ExpressionBody
LensTH.makeLenses ''ListItemActions
LensTH.makeLenses ''FuncParamActions
LensTH.makeLenses ''Payload
LensTH.makeLenses ''ExpressionP
LensTH.makeLenses ''HoleResult
LensTH.makeLenses ''HoleActions
LensTH.makeLenses ''Hole
