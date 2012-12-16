{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Lamdu.CodeEdit.Sugar.Types
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
  , ExpressionBody(..), eHasParens
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..), rGuid, rExpressionBody, rPayload
  , Expression
  , WhereItem(..)
  , Func(..), fDepParams, fParams, fBody
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), HoleActions(..), HoleResult
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
import Lamdu.Data.IRef (DefI)
import qualified Control.Lens.TH as LensTH
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.Infer as Infer

type T = Transaction
type CT m = StateT Cache (T m)

data Actions m = Actions
  { giveAsArg    :: T m Guid
  , replace      :: T m Guid
  , cut          :: T m Guid
  -- Turn "x" to "x ? _" where "?" is an operator-hole.
  -- Given string is initial hole search term.
  , giveAsArgToOperator :: String -> T m Guid
  }

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
  } deriving (Functor)

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

type HoleResult t = Data.Expression (DefI t) (Infer.Inferred (DefI t))

data HoleActions m = HoleActions
  { holePickResult :: HoleResult (Tag m) -> T m (Guid, Actions m)
  , holeConvertResult :: HoleResult (Tag m) -> T m (Expression m)
  , holePaste :: Maybe (T m Guid)
  }

data Hole m = Hole
  { holeScope :: [Guid]
  , holeInferResults :: Data.Expression (DefI (Tag m)) () -> CT m [HoleResult (Tag m)]
  , holeMActions :: Maybe (HoleActions m)
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred m expr = Inferred
  { iValue :: expr
  , iHole :: Hole m
  } deriving (Functor)

data Polymorphic t expr = Polymorphic
  { pFuncGuid :: Guid
  , pCompact :: Data.VariableRef (DefI t)
  , pFullExpression :: expr
  } deriving (Functor)

data ExpressionBody m expr
  = ExpressionApply   { _eHasParens :: HasParens, __eApply :: Data.Apply expr }
  | ExpressionSection { _eHasParens :: HasParens, __eSection :: Section expr }
  | ExpressionFunc    { _eHasParens :: HasParens, __eFunc :: Func m expr }
  | ExpressionPi      { _eHasParens :: HasParens, __ePi :: Pi m expr }
  | ExpressionGetVariable { __getVariable :: Data.VariableRef (DefI (Tag m)) }
  | ExpressionHole { __eHole :: Hole m }
  | ExpressionInferred { __eInferred :: Inferred m expr }
  | ExpressionPolymorphic { __ePolymorphic :: Polymorphic (Tag m) expr }
  | ExpressionLiteralInteger { __eLit :: LiteralInteger m }
  | ExpressionAtom { __eAtom :: String }
  deriving (Functor)

wrapParens :: HasParens -> String -> String
wrapParens HaveParens x = concat ["(", x, ")"]
wrapParens DontHaveParens x = x

instance Show expr => Show (FuncParam m expr) where
  show fp =
    concat ["(", show (_fpGuid fp), ":", show (_fpType fp), ")"]

instance Show expr => Show (ExpressionBody m expr) where
  show ExpressionApply   { _eHasParens = hasParens, __eApply = Data.Apply func arg } =
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
  show ExpressionGetVariable { __getVariable = Data.ParameterRef guid } = 'P' : show guid
  show ExpressionGetVariable { __getVariable = Data.DefinitionRef defI } = 'D' : show (IRef.guid defI)
  show ExpressionHole {} = "Hole"
  show ExpressionInferred {} = "Inferred"
  show ExpressionPolymorphic {} = "Poly"
  show ExpressionLiteralInteger { __eLit = LiteralInteger i _ } = show i
  show ExpressionAtom { __eAtom = atom } = atom

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
  { biName :: Data.FFIName
  -- Consider removing Maybe'ness here
  , biMSetName :: Maybe (Data.FFIName -> T m ())
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
