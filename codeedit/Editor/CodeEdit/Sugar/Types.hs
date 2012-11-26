{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Editor.CodeEdit.Sugar.Types
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
  , Func(..)
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), HoleActions(..), HoleResult
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  -- Internal to Sugar:
  , ExprEntity, ExprEntityStored, eeGuid, eeStored, eeProp, eeFromPure
  , ExprEntityInferred(..), eeiGuid
  , InferExpressionResult(..), ierContextHash, ierExpr, ierRefmap, ierSuccess
  , EntityPayload(..)
  , T, CT
  ) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary(..))
import Data.Cache (Cache)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Data.Cache as Cache
import qualified Data.Store.IRef as IRef
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer
import qualified System.Random.Utils as RandomUtils

type T = Transaction ViewTag
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
  { fParams :: [FuncParam m expr]
  , fBody :: expr
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

type HoleResult = Infer.Expression ()

data HoleActions m = HoleActions
  { holePickResult :: HoleResult -> T m (Guid, Actions m)
  , holeConvertResult :: HoleResult -> T m (Expression m)
  , holePaste :: Maybe (T m Guid)
  }

data Hole m = Hole
  { holeScope :: [Guid]
  , holeInferResults :: Data.PureExpression -> CT m [HoleResult]
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

data Polymorphic expr = Polymorphic
  { pFuncGuid :: Guid
  , pCompact :: Data.VariableRef
  , pFullExpression :: expr
  } deriving (Functor)

data ExpressionBody m expr
  = ExpressionApply   { _eHasParens :: HasParens, __eApply :: Data.Apply expr }
  | ExpressionSection { _eHasParens :: HasParens, __eSection :: Section expr }
  | ExpressionFunc    { _eHasParens :: HasParens, __eFunc :: Func m expr }
  | ExpressionPi      { _eHasParens :: HasParens, __ePi :: Pi m expr }
  | ExpressionGetVariable { __getVariable :: Data.VariableRef }
  | ExpressionHole { __eHole :: Hole m }
  | ExpressionInferred { __eInferred :: Inferred m expr }
  | ExpressionPolymorphic { __ePolymorphic :: Polymorphic expr }
  | ExpressionLiteralInteger { __eLit :: LiteralInteger m }
  | ExpressionAtom { __eAtom :: String }
  deriving (Functor)

wrapParens :: HasParens -> String -> String
wrapParens HaveParens x = concat ["(", x, ")"]
wrapParens DontHaveParens x = x

instance Show expr => Show (ExpressionBody m expr) where
  show ExpressionApply   { _eHasParens = hasParens, __eApply = Data.Apply func arg } =
    wrapParens hasParens $ show func ++ " " ++ show arg
  show ExpressionSection { _eHasParens = hasParens, __eSection = Section mleft op mright } =
    wrapParens hasParens $ maybe "" show mleft ++ " " ++ show op ++ maybe "" show mright
  show ExpressionFunc    { _eHasParens = hasParens, __eFunc = Func params body } =
    wrapParens hasParens $ "\\" ++ unwords (map show params) ++ " -> " ++ show body
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
  , wiActions :: ListItemActions m
  }

-- Common data for definitions and where-items
data DefinitionContent m = DefinitionContent
  { dBody :: Expression m
  , dParameters :: [FuncParam m (Expression m)]
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

LensTH.makeLenses ''FuncParam
LensTH.makeLenses ''ExpressionBody
LensTH.makeLenses ''ListItemActions
LensTH.makeLenses ''FuncParamActions
LensTH.makeLenses ''Payload
LensTH.makeLenses ''ExpressionP

instance Show expr => Show (FuncParam m expr) where
  show fp =
    concat ["(", show (fp ^. fpHiddenLambdaGuid), ":", show (fp ^. fpType), ")"]

data ExprEntityInferred a = ExprEntityInferred
  { eeiInferred :: Infer.Inferred a
  , eeiTypeConflicts :: [Data.PureExpression]
  , eeiValueConflicts :: [Data.PureExpression]
  } deriving (Functor, Foldable, Traversable)
derive makeBinary ''ExprEntityInferred

type ExprEntityStored m =
  ExprEntityInferred (DataIRef.ExpressionProperty (T m))

type ExprEntityMStored m =
  ExprEntityInferred (Maybe (DataIRef.ExpressionProperty (T m)))

data InferExpressionResult m = InferExpressionResult
  { _ierSuccess :: Bool
  , _ierRefmap :: Infer.RefMap
  , _ierExpr :: Data.Expression (ExprEntityStored m)
  , _ierContextHash :: Cache.KeyBS
  }
LensTH.makeLenses ''InferExpressionResult

data EntityPayload m = EntityPayload
  { eplGuid :: Guid
  , eplInferred :: Maybe (ExprEntityMStored m)
  }

eeiGuid :: ExprEntityStored m -> Guid
eeiGuid = DataIRef.epGuid . Infer.iStored . eeiInferred

eeGuid :: ExprEntity m -> Guid
eeGuid = eplGuid . Lens.view Data.ePayload

type ExprEntity m = Data.Expression (EntityPayload m)

eeStored :: ExprEntity m -> Maybe (ExprEntityStored m)
eeStored = Traversable.sequenceA <=< eplInferred . Lens.view Data.ePayload

eeProp :: ExprEntity m -> Maybe (DataIRef.ExpressionProperty (T m))
eeProp = Infer.iStored . eeiInferred <=< eplInferred . Lens.view Data.ePayload

eeFromPure :: RandomGen g => g -> Data.PureExpression -> ExprEntity m
eeFromPure gen =
    Data.randomizeParamIds paramGen
  . Data.randomizeExpr exprGen
  . (fmap . const) (`EntityPayload` Nothing)
  where
    paramGen : exprGen : _ = RandomUtils.splits gen
