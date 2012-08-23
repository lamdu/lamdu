{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Editor.Data
  ( Definition(..), DefinitionBody(..)
  , DefinitionI, DefinitionIRef
  , FFIName(..)
  , VariableRef(..), variableRefGuid
  , Lambda(..), atLambdaParamType, atLambdaBody
  , LambdaI
  , Apply(..), atApplyFunc, atApplyArg
  , ApplyI
  , Builtin(..)
  , Leaf(..)
  , ExpressionBody(..)
  , makeApply, makePi, makeLambda
  , makeParameterRef
  , ExpressionIRefProperty, eipGuid
  , ExpressionI, ExpressionIRef(..)
  , Expression(..), atEGuid, atEValue, atEPayload
  , PureExpression, pureExpression
  , newExprIRef, readExprIRef, writeExprIRef, exprIRefGuid
  , newIRefExpressionFromPure, writeIRefExpressionFromPure
  , canonizeGuids, randomizeGuids
  , matchExpressionBody
  , matchExpression
  ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Monad ((<=<), guard, liftM, liftM2, mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.Derive.Foldable (makeFoldable)
import Data.Derive.Traversable (makeTraversable)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable(..))
import System.Random (RandomGen, random)
import Prelude hiding (mapM, sequence)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified System.Random as Random

type ExpressionIRefProperty m = Property m ExpressionIRef

eipGuid :: ExpressionIRefProperty m -> Guid
eipGuid = IRef.guid . unExpressionIRef . Property.value

newtype ExpressionIRef = ExpressionIRef {
  unExpressionIRef :: IRef (ExpressionBody ExpressionIRef)
  } deriving (Eq, Ord, Show)

exprIRefGuid :: ExpressionIRef -> Guid
exprIRefGuid = IRef.guid . unExpressionIRef

newExprIRef
  :: Monad m
  => ExpressionBody ExpressionIRef -> Transaction t m ExpressionIRef
newExprIRef = liftM ExpressionIRef . Transaction.newIRef

readExprIRef
  :: Monad m
  => ExpressionIRef -> Transaction t m (ExpressionBody ExpressionIRef)
readExprIRef = Transaction.readIRef . unExpressionIRef

writeExprIRef
  :: Monad m
  => ExpressionIRef -> ExpressionBody ExpressionIRef -> Transaction t m ()
writeExprIRef = Transaction.writeIRef . unExpressionIRef

data Lambda expr = Lambda {
  lambdaParamType :: expr,
  lambdaBody :: expr
  } deriving (Eq, Ord, Show, Functor)
instance Applicative Lambda where
  pure x = Lambda x x
  Lambda p0 b0 <*> Lambda p1 b1 = Lambda (p0 p1) (b0 b1)
type LambdaI = Lambda ExpressionIRef

data Apply expr = Apply {
  applyFunc :: expr,
  applyArg :: expr
  } deriving (Eq, Ord, Show, Functor)
instance Applicative Apply where
  pure x = Apply x x
  Apply f0 a0 <*> Apply f1 a1 = Apply (f0 f1) (a0 a1)
type ApplyI = Apply ExpressionIRef

data VariableRef
  = ParameterRef Guid -- of the lambda/pi
  | DefinitionRef DefinitionIRef
  deriving (Eq, Ord, Show)

data Leaf
  = GetVariable VariableRef
  | LiteralInteger Integer
  | Set
  | IntegerType
  | Hole
  deriving (Eq, Ord, Show)

data ExpressionBody expr
  = ExpressionLambda (Lambda expr)
  | ExpressionPi (Lambda expr)
  | ExpressionApply (Apply expr)
  | ExpressionLeaf Leaf
  deriving (Eq, Ord, Functor)
type ExpressionI = ExpressionBody ExpressionIRef

makeApply :: expr -> expr -> ExpressionBody expr
makeApply func arg = ExpressionApply $ Apply func arg

makePi :: expr -> expr -> ExpressionBody expr
makePi argType resultType = ExpressionPi $ Lambda argType resultType

makeLambda :: expr -> expr -> ExpressionBody expr
makeLambda argType body = ExpressionLambda $ Lambda argType body

makeParameterRef :: Guid -> ExpressionBody a
makeParameterRef = ExpressionLeaf . GetVariable . ParameterRef

instance Show expr => Show (ExpressionBody expr) where
  show (ExpressionLambda (Lambda paramType body)) = concat ["\\:", showP paramType, "==>", showP body]
  show (ExpressionPi (Lambda paramType body)) = concat [showP paramType, "->", showP body]
  show (ExpressionApply (Apply func arg)) = unwords [showP func, showP arg]
  show (ExpressionLeaf (GetVariable (ParameterRef guid))) = "par:" ++ show guid
  show (ExpressionLeaf (GetVariable (DefinitionRef defI))) = "def:" ++ show (IRef.guid defI)
  show (ExpressionLeaf (LiteralInteger int)) = show int
  show (ExpressionLeaf x) = show x

showP :: Show a => a -> String
showP = parenify . show

parenify :: String -> String
parenify x = concat ["(", x, ")"]

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord)

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Builtin expr = Builtin
  { bName :: FFIName
  , bType :: expr
  } deriving (Eq, Ord, Show)

data DefinitionBody expr
  = DefinitionExpression { defExpression :: expr }
  | DefinitionBuiltin (Builtin expr)
  deriving (Eq, Ord, Show)

data Definition expr = Definition
  { defBody :: DefinitionBody expr
  , defType :: expr
  } deriving (Eq, Ord, Show)
type DefinitionI = Definition ExpressionIRef
type DefinitionIRef = IRef DefinitionI

type PureExpression = Expression ()

data Expression a = Expression
  { eGuid :: Guid
  , eValue :: ExpressionBody (Expression a)
  , ePayload :: a
  } deriving (Functor, Eq, Ord)

instance Show a => Show (Expression a) where
  show (Expression guid value payload) = show guid ++ ":" ++ show value ++ "{" ++ show payload ++ "}"

AtFieldTH.make ''Expression

pureExpression :: Guid -> ExpressionBody PureExpression -> PureExpression
pureExpression guid body = Expression guid body ()

variableRefGuid :: VariableRef -> Guid
variableRefGuid (ParameterRef i) = i
variableRefGuid (DefinitionRef i) = IRef.guid i

hasLambda :: ExpressionBody expr -> Bool
hasLambda ExpressionPi {} = True
hasLambda ExpressionLambda {} = True
hasLambda _ = False

type Scope = [(Guid, Guid)]

newIRefExpressionFromPure
  :: Monad m => PureExpression -> Transaction t m ExpressionIRef
newIRefExpressionFromPure =
  newIRefExpressionFromPureH []

newIRefExpressionFromPureH :: Monad m => Scope -> PureExpression -> Transaction t m ExpressionIRef
newIRefExpressionFromPureH scope =
  liftM ExpressionIRef . Transaction.newIRefWithGuid .
  flip (expressionIFromPure scope)

writeIRefExpressionFromPure
  :: Monad m => ExpressionIRef -> PureExpression -> Transaction t m ()
writeIRefExpressionFromPure (ExpressionIRef iref) =
  Transaction.writeIRef iref <=< expressionIFromPure [] (IRef.guid iref)

expressionIFromPure
  :: Monad m => Scope -> Guid -> PureExpression -> Transaction t m ExpressionI
expressionIFromPure scope newGuid (Expression oldGuid expr ()) =
  mapM (newIRefExpressionFromPureH newScope) newExpr
  where
    newScope
      | hasLambda expr = (oldGuid, newGuid) : scope
      | otherwise = scope
    newExpr = case expr of
      ExpressionLeaf (GetVariable (ParameterRef parGuid)) ->
        makeParameterRef . fromMaybe parGuid $ lookup parGuid newScope
      x -> x

randomizeGuids ::
  RandomGen g => g -> Expression a -> Expression a
randomizeGuids gen =
  (`evalState` gen) . (`runReaderT` Map.empty) . go
  where
    onLambda oldGuid newGuid (Lambda paramType body) =
      liftM2 Lambda (go paramType) .
      Reader.local (Map.insert oldGuid newGuid) $ go body
    go (Expression oldGuid v s) = do
      newGuid <- lift $ state random
      liftM (flip (Expression newGuid) s) $
        case v of
        ExpressionLambda lambda ->
          liftM ExpressionLambda $ onLambda oldGuid newGuid lambda
        ExpressionPi lambda ->
          liftM ExpressionPi $ onLambda oldGuid newGuid lambda
        ExpressionApply (Apply func arg) ->
          liftM2 makeApply (go func) (go arg)
        gv@(ExpressionLeaf (GetVariable (ParameterRef guid))) ->
          Reader.asks $
          maybe gv makeParameterRef .
          Map.lookup guid
        _ -> return v

canonizeGuids :: Expression a -> Expression a
canonizeGuids = randomizeGuids $ Random.mkStdGen 0

matchExpressionBody ::
  (a -> b -> c) -> ExpressionBody a -> ExpressionBody b -> Maybe (ExpressionBody c)
matchExpressionBody f (ExpressionLambda l0) (ExpressionLambda l1) =
  Just . ExpressionLambda $ liftA2 f l0 l1
matchExpressionBody f (ExpressionPi l0) (ExpressionPi l1) =
  Just . ExpressionPi $ liftA2 f l0 l1
matchExpressionBody f (ExpressionApply a0) (ExpressionApply a1) =
  Just . ExpressionApply $ liftA2 f a0 a1
matchExpressionBody _ (ExpressionLeaf v0) (ExpressionLeaf v1)
  | v0 == v1 = Just $ ExpressionLeaf v0
matchExpressionBody _ _ _ = Nothing

matchExpression ::
  (a -> b -> c) -> Expression a -> Expression b -> Maybe (Expression c)
matchExpression f e0 e1 =
  runReaderT (go e0 e1) (Map.empty)
  where
    go (Expression g0 body0 val0) (Expression g1 body1 val1) =
      fmap (flip (Expression g0) (f val0 val1)) .
      Reader.local (Map.insert g1 g0) $
      case matchExpressionBody go body0 body1 of
      Just x -> sequence x
      Nothing ->
        case (body0, body1) of
        (ExpressionLeaf l@(GetVariable (ParameterRef par0)),
         ExpressionLeaf (GetVariable (ParameterRef par1))) -> do
          guard . (par0 ==) . fromMaybe par1 =<< Reader.asks (Map.lookup par1)
          return $ ExpressionLeaf l
        _ -> mzero

derive makeFoldable ''Apply
derive makeFoldable ''Lambda
derive makeFoldable ''ExpressionBody
derive makeFoldable ''Expression
derive makeTraversable ''Apply
derive makeTraversable ''Lambda
derive makeTraversable ''ExpressionBody
derive makeTraversable ''Expression
derive makeBinary ''ExpressionIRef
derive makeBinary ''FFIName
derive makeBinary ''VariableRef
derive makeBinary ''Lambda
derive makeBinary ''Apply
derive makeBinary ''Leaf
derive makeBinary ''ExpressionBody
derive makeBinary ''Expression
derive makeBinary ''Builtin
derive makeBinary ''DefinitionBody
derive makeBinary ''Definition
AtFieldTH.make ''Lambda
AtFieldTH.make ''Apply
AtFieldTH.make ''Definition
