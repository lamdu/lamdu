{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Editor.Data
  ( Definition(..), atDefBody
  , DefinitionI, DefinitionIRef
  , FFIName(..)
  , VariableRef(..), variableRefGuid
  , Lambda(..), atLambdaParamType, atLambdaBody
  , LambdaI
  , Apply(..), atApplyFunc, atApplyArg
  , ApplyI
  , Builtin(..)
  , Expression(..), makeApply, makePi, makeLambda
  , ExpressionIRefProperty, eipGuid
  , ExpressionI, ExpressionIRef(..)
  , GuidExpression(..), atGeGuid, atGeValue
  , PureGuidExpression(..), pureGuidExpression, atPureGuidExpression
  , newExprIRef, readExprIRef, writeExprIRef, exprIRefGuid
  , newIRefExpressionFromPure, writeIRefExpressionFromPure
  , mapMExpression
  , canonizeIdentifiers
  , matchExpression
  ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Monad (liftM, liftM2, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.Derive.Foldable (makeFoldable)
import Data.Derive.Traversable (makeTraversable)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable(..))
import Prelude hiding (mapM)
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
  unExpressionIRef :: IRef (Expression ExpressionIRef)
  } deriving (Eq, Ord, Show)

exprIRefGuid :: ExpressionIRef -> Guid
exprIRefGuid = IRef.guid . unExpressionIRef

newExprIRef
  :: Monad m
  => Expression ExpressionIRef -> Transaction t m ExpressionIRef
newExprIRef = liftM ExpressionIRef . Transaction.newIRef

readExprIRef
  :: Monad m
  => ExpressionIRef -> Transaction t m (Expression ExpressionIRef)
readExprIRef = Transaction.readIRef . unExpressionIRef

writeExprIRef
  :: Monad m
  => ExpressionIRef -> Expression ExpressionIRef -> Transaction t m ()
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

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord)

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Builtin expr = Builtin
  { bName :: FFIName
  , bType :: expr
  } deriving (Eq, Ord, Show, Functor)

data Expression expr
  = ExpressionLambda (Lambda expr)
  | ExpressionPi (Lambda expr)
  | ExpressionApply (Apply expr)
  | ExpressionGetVariable VariableRef
  | ExpressionHole
  | ExpressionLiteralInteger Integer
  | ExpressionBuiltin (Builtin expr)
  | ExpressionSet
  deriving (Eq, Ord, Functor)
type ExpressionI = Expression ExpressionIRef

makeApply :: expr -> expr -> Expression expr
makeApply func arg = ExpressionApply $ Apply func arg

makePi :: expr -> expr -> Expression expr
makePi argType resultType = ExpressionPi $ Lambda argType resultType

makeLambda :: expr -> expr -> Expression expr
makeLambda argType body = ExpressionLambda $ Lambda argType body

instance Show expr => Show (Expression expr) where
  show (ExpressionLambda (Lambda paramType body)) = concat ["\\:", showP paramType, "==>", showP body]
  show (ExpressionPi (Lambda paramType body)) = concat [showP paramType, "->", showP body]
  show (ExpressionApply (Apply func arg)) = unwords [showP func, showP arg]
  show (ExpressionGetVariable (ParameterRef guid)) = "par:" ++ show guid
  show (ExpressionGetVariable (DefinitionRef defI)) = "def:" ++ show (IRef.guid defI)
  show (ExpressionLiteralInteger int) = show int
  show (ExpressionBuiltin (Builtin name _)) = show name
  show ExpressionHole = "Hole"
  show ExpressionSet = "Set"

showP :: Show a => a -> String
showP = parenify . show

parenify :: String -> String
parenify x = concat ["(", x, ")"]

data Definition expr = Definition
  { defBody :: expr
  , defType :: expr
  } deriving (Eq, Ord, Show)
type DefinitionI = Definition ExpressionIRef
type DefinitionIRef = IRef DefinitionI

newtype PureGuidExpression = PureGuidExpression
  { unPureGuidExpression :: GuidExpression PureGuidExpression
  } deriving (Eq)

instance Show PureGuidExpression where
  show = show . unPureGuidExpression

data GuidExpression ref = GuidExpression
  { geGuid :: Guid
  , geValue :: Expression ref
  } deriving (Functor, Eq)

instance Show ref => Show (GuidExpression ref) where
  show (GuidExpression guid value) = show guid ++ ":" ++ show value

AtFieldTH.make ''PureGuidExpression
AtFieldTH.make ''GuidExpression

pureGuidExpression :: Guid -> Expression PureGuidExpression -> PureGuidExpression
pureGuidExpression guid = PureGuidExpression . GuidExpression guid

variableRefGuid :: VariableRef -> Guid
variableRefGuid (ParameterRef i) = i
variableRefGuid (DefinitionRef i) = IRef.guid i

mapMExpression
  :: Monad m
  => (from
      -> ( m (Expression from)
         , Expression to -> m to ))
  -> from -> m to
mapMExpression f src =
  afterRecurse =<< mapM (mapMExpression f) =<< makeExpr
  where
    (makeExpr, afterRecurse) = f src

hasLambda :: Expression expr -> Bool
hasLambda ExpressionPi {} = True
hasLambda ExpressionLambda {} = True
hasLambda _ = False

type Scope = [(Guid, Guid)]

newIRefExpressionFromPure
  :: Monad m => PureGuidExpression -> Transaction t m ExpressionIRef
newIRefExpressionFromPure =
  newIRefExpressionFromPureH []

newIRefExpressionFromPureH :: Monad m => Scope -> PureGuidExpression -> Transaction t m ExpressionIRef
newIRefExpressionFromPureH scope =
  liftM ExpressionIRef . Transaction.newIRefWithGuid .
  flip (expressionIFromPure scope)

writeIRefExpressionFromPure
  :: Monad m => ExpressionIRef -> PureGuidExpression -> Transaction t m ()
writeIRefExpressionFromPure (ExpressionIRef iref) =
  Transaction.writeIRef iref <=< expressionIFromPure [] (IRef.guid iref)

expressionIFromPure
  :: Monad m => Scope -> Guid -> PureGuidExpression -> Transaction t m ExpressionI
expressionIFromPure scope newGuid (PureGuidExpression (GuidExpression oldGuid expr)) =
  mapM (newIRefExpressionFromPureH newScope) newExpr
  where
    newScope
      | hasLambda expr = (oldGuid, newGuid) : scope
      | otherwise = scope
    newExpr = case expr of
      ExpressionGetVariable (ParameterRef parGuid) ->
        ExpressionGetVariable . ParameterRef .
        fromMaybe parGuid $ lookup parGuid newScope
      x -> x

canonizeIdentifiers
  :: Random.RandomGen g => g -> PureGuidExpression -> PureGuidExpression
canonizeIdentifiers gen =
  runIdentity . runRandomT gen . (`runReaderT` Map.empty) . go
  where
    onLambda oldGuid newGuid (Lambda paramType body) =
      liftM2 Lambda (go paramType) .
      Reader.local (Map.insert oldGuid newGuid) $ go body
    go (PureGuidExpression (GuidExpression oldGuid v)) = do
      newGuid <- lift nextRandom
      liftM (pureGuidExpression newGuid) $
        case v of
        ExpressionLambda lambda ->
          liftM ExpressionLambda $ onLambda oldGuid newGuid lambda
        ExpressionPi lambda ->
          liftM ExpressionPi $ onLambda oldGuid newGuid lambda
        ExpressionApply (Apply func arg) ->
          liftM2 makeApply (go func) (go arg)
        gv@(ExpressionGetVariable (ParameterRef guid)) ->
          Reader.asks $
          maybe gv (ExpressionGetVariable . ParameterRef) .
          Map.lookup guid
        x -> return x

matchExpression ::
  (a -> b -> c) -> Expression a -> Expression b -> Maybe (Expression c)
matchExpression f (ExpressionLambda l0) (ExpressionLambda l1) =
  Just . ExpressionLambda $ liftA2 f l0 l1
matchExpression f (ExpressionPi l0) (ExpressionPi l1) =
  Just . ExpressionPi $ liftA2 f l0 l1
matchExpression f (ExpressionApply a0) (ExpressionApply a1) =
  Just . ExpressionApply $ liftA2 f a0 a1
matchExpression _
  (ExpressionGetVariable v0)
  (ExpressionGetVariable v1)
  | v0 == v1 = Just $ ExpressionGetVariable v0
matchExpression _ ExpressionHole ExpressionHole =
  Just ExpressionHole
matchExpression _
  (ExpressionLiteralInteger i0)
  (ExpressionLiteralInteger i1)
  | i0 == i1 = Just $ ExpressionLiteralInteger i0
matchExpression f
  (ExpressionBuiltin (Builtin n0 t0))
  (ExpressionBuiltin (Builtin n1 t1))
  | n0 == n1 = Just . ExpressionBuiltin . Builtin n0 $ f t0 t1
matchExpression _ ExpressionSet ExpressionSet =
  Just ExpressionSet
matchExpression _ _ _ = Nothing

derive makeFoldable ''Builtin
derive makeFoldable ''Apply
derive makeFoldable ''Lambda
derive makeFoldable ''Expression
derive makeFoldable ''GuidExpression
derive makeTraversable ''Builtin
derive makeTraversable ''Apply
derive makeTraversable ''Lambda
derive makeTraversable ''Expression
derive makeTraversable ''GuidExpression
derive makeBinary ''PureGuidExpression
derive makeBinary ''GuidExpression
derive makeBinary ''ExpressionIRef
derive makeBinary ''FFIName
derive makeBinary ''VariableRef
derive makeBinary ''Lambda
derive makeBinary ''Apply
derive makeBinary ''Builtin
derive makeBinary ''Expression
derive makeBinary ''Definition
AtFieldTH.make ''Lambda
AtFieldTH.make ''Apply
AtFieldTH.make ''Definition
