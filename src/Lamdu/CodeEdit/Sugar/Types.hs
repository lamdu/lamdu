{-# LANGUAGE TemplateHaskell, DeriveFunctor, KindSignatures #-}
module Lamdu.CodeEdit.Sugar.Types
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..), giveAsArg, callWithArg, callWithNextArg, replace, cut, giveAsArgToOperator
  , ExpressionBody(..), eHasParens
    , expressionPi, expressionApply, expressionSection
    , expressionFunc, expressionGetVariable, expressionHole
    , expressionInferred, expressionPolymorphic
    , expressionLiteralInteger, expressionAtom
    , expressionList
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..), rGuid, rExpressionBody, rPayload, rHiddenGuids
  , Expression
  , WhereItem(..)
  , ListItem(..), List(..)
  , Func(..), fDepParams, fParams, fBody
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), holeScope, holeMActions, holeInferResults
  , HoleActions(..), holePaste, holeMDelete
  , HoleResult(..)
    , holeResultInferred
    , holeResultConvert, holeResultMPickAndCallWithArg
    , holeResultPick, holeResultPickAndGiveAsArg
    , holeResultPickAndGiveAsArgToOperator
    , holeResultMPickAndCallWithNextArg
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  , T, CT
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Cache (Cache)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
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

data Actions m = Actions
  { _giveAsArg :: T m Guid
  -- Turn "x" to "x ? _" where "?" is an operator-hole.
  -- Given string is initial hole search term.
  , _giveAsArgToOperator :: String -> T m Guid
  , _callWithNextArg :: T m (Maybe (T m Guid))
  , _callWithArg :: T m (Maybe (T m Guid))
  , _replace :: T m Guid
  , _cut :: T m Guid
  }
LensTH.makeLenses ''Actions

data HasParens = HaveParens | DontHaveParens

data Payload m = Payload
  { _plInferredTypes :: [Expression m]
  , _plActions :: Maybe (Actions m)
  , _plNextHole :: Maybe (Expression m)
  }

data ExpressionP m pl = Expression
  { _rGuid :: Guid
  , _rExpressionBody :: ExpressionBody m (ExpressionP m pl)
  , _rPayload :: pl
  , -- Guids from data model expression which were sugared out into
    -- this sugar expression.
    -- If the cursor was on them for whatever reason, it should be
    -- mapped into the sugar expression's guid.
    _rHiddenGuids :: [Guid]
  } deriving (Functor)

type Expression m = ExpressionP m (Payload m)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m Guid
  , _itemDelete :: T m Guid
  }

data FuncParamActions m expr = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  , _fpGetExample :: CT m expr
  } deriving (Functor)

data FuncParam m expr = FuncParam
  { _fpGuid :: Guid
  , _fpHiddenLambdaGuid :: Maybe Guid
  , _fpType :: expr
  , _fpMActions :: Maybe (FuncParamActions m expr)
  } deriving (Functor)

-- Multi-param Lambda
data Func m expr = Func
  { _fDepParams :: [FuncParam m expr]
  , _fParams :: [FuncParam m expr]
  , _fBody :: expr
  } deriving (Functor)

data Pi m expr = Pi
  { pParam :: FuncParam m expr
  , pResultType :: expr
  } deriving (Functor)

-- Infix Sections include: (+), (1+), (+1), (1+2). Last is really just
-- infix application, but considered an infix section too.
data Section expr = Section
  { sectionLArg :: Maybe expr
  , sectionOp :: expr -- TODO: Always a Data.GetVariable, use a more specific type
  , sectionRArg :: Maybe expr
  } deriving (Functor)

data HoleResult m expr = HoleResult
  { _holeResultInferred :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))
  , _holeResultConvert :: T m expr
  , _holeResultPick :: T m Guid
  , _holeResultPickAndGiveAsArg :: T m Guid
  , _holeResultPickAndGiveAsArgToOperator :: String -> T m Guid
  , _holeResultMPickAndCallWithArg :: T m (Maybe (T m Guid))
  , _holeResultMPickAndCallWithNextArg :: T m (Maybe (T m Guid))
  } deriving (Functor)

data HoleActions m expr = HoleActions
  { _holeScope :: [Guid]
  , _holeInferResults :: DataIRef.ExpressionM m () -> CT m [HoleResult m expr]
  , _holePaste :: Maybe (T m Guid)
  , -- TODO: holeMDelete is always Nothing, not implemented yet
    _holeMDelete :: Maybe (T m Guid)
  } deriving (Functor)

newtype Hole m expr = Hole
  { _holeMActions :: Maybe (HoleActions m expr)
  } deriving (Functor)

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred m expr = Inferred
  { iValue :: expr
  , iHole :: Hole m expr
  } deriving (Functor)

data Polymorphic t expr = Polymorphic
  { pFuncGuid :: Guid
  , pCompact :: Expression.VariableRef (DefI t)
  , pFullExpression :: expr
  } deriving (Functor)

-- TODO: Do we want to store/allow-access to the implicit type params (nil's type, each cons type?)
data ListItem m expr = ListItem
  { liMActions :: Maybe (ListItemActions m)
  , liExpr :: expr
  } deriving (Functor)

data List m expr = List
  { lValues :: [ListItem m expr]
  , lMAddFirstItem :: Maybe (T m Guid)
  } deriving (Functor)

data ExpressionBody m expr
  = ExpressionApply   { _eHasParens :: HasParens, __eApply :: Expression.Apply expr }
  | ExpressionSection { _eHasParens :: HasParens, __eSection :: Section expr }
  | ExpressionFunc    { _eHasParens :: HasParens, __eFunc :: Func m expr }
  | ExpressionPi      { _eHasParens :: HasParens, __ePi :: Pi m expr }
  | ExpressionGetVariable { __getVariable :: Expression.VariableRef (DefI (Tag m)) }
  | ExpressionHole    { __eHole :: Hole m expr }
  | ExpressionInferred { __eInferred :: Inferred m expr }
  | ExpressionPolymorphic { __ePolymorphic :: Polymorphic (Tag m) expr }
  | ExpressionLiteralInteger { __eLit :: LiteralInteger m }
  | ExpressionAtom { __eAtom :: String }
  | ExpressionList { __eList :: List m expr }
  deriving (Functor)
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
