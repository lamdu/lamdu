{-# LANGUAGE TemplateHaskell #-}
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
  , Expression(..)
  , ExpressionIRefProperty
  , ExpressionI, ExpressionIRef(..)
  , GuidExpression(..), atGeGuid, atGeValue
  , PureGuidExpression(..), atPureGuidExpression
  , newExprIRef, readExprIRef, writeExprIRef, exprIRefGuid
  , newIRefExpressionFromPure, writeIRefExpressionFromPure
  , mapMExpression
  , mapExpression, sequenceExpression
  , canonizeIdentifiers
  ) where

import Control.Monad (liftM, liftM2, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Store.IRef(IRef)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified System.Random as Random

type ExpressionIRefProperty m = Property m ExpressionIRef

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
  } deriving (Eq, Ord, Show)
type LambdaI = Lambda ExpressionIRef

data Apply expr = Apply {
  applyFunc :: expr,
  applyArg :: expr
  } deriving (Eq, Ord, Show)
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
  } deriving (Eq, Ord, Show)

data Expression expr
  = ExpressionLambda (Lambda expr)
  | ExpressionPi (Lambda expr)
  | ExpressionApply (Apply expr)
  | ExpressionGetVariable VariableRef
  | ExpressionHole
  | ExpressionLiteralInteger Integer
  | ExpressionBuiltin (Builtin expr)
  | ExpressionMagic
  deriving (Eq, Ord, Show)
type ExpressionI = Expression ExpressionIRef

data Definition expr = Definition
  { defBody :: expr
  , defType :: expr
  } deriving (Eq, Ord, Show)
type DefinitionI = Definition ExpressionIRef
type DefinitionIRef = IRef DefinitionI


newtype PureGuidExpression = PureGuidExpression
  { unPureGuidExpression :: GuidExpression PureGuidExpression
  } deriving (Show, Eq)

data GuidExpression ref = GuidExpression
  { geGuid :: Guid
  , geValue :: Expression ref
  } deriving (Show, Eq)

AtFieldTH.make ''PureGuidExpression
AtFieldTH.make ''GuidExpression
derive makeBinary ''PureGuidExpression
derive makeBinary ''GuidExpression

variableRefGuid :: VariableRef -> Guid
variableRefGuid (ParameterRef i) = i
variableRefGuid (DefinitionRef i) = IRef.guid i

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

mapExpression :: (a -> b) -> Expression a -> Expression b
mapExpression f (ExpressionLambda (Lambda x y)) = ExpressionLambda $ Lambda (f x) (f y)
mapExpression f (ExpressionPi (Lambda x y)) = ExpressionPi $ Lambda (f x) (f y)
mapExpression f (ExpressionApply (Apply x y)) = ExpressionApply $ Apply (f x) (f y)
mapExpression f (ExpressionBuiltin (Builtin name t)) = ExpressionBuiltin . Builtin name $ f t
mapExpression _ (ExpressionGetVariable var) = ExpressionGetVariable var
mapExpression _ ExpressionHole = ExpressionHole
mapExpression _ (ExpressionLiteralInteger int) = ExpressionLiteralInteger int
mapExpression _ ExpressionMagic = ExpressionMagic

sequenceExpression :: Monad f => Expression (f a) -> f (Expression a)
sequenceExpression (ExpressionLambda (Lambda x y)) = liftM ExpressionLambda $ liftM2 Lambda x y
sequenceExpression (ExpressionPi (Lambda x y)) = liftM ExpressionPi $ liftM2 Lambda x y
sequenceExpression (ExpressionApply (Apply x y)) = liftM ExpressionApply $ liftM2 Apply x y
sequenceExpression (ExpressionBuiltin (Builtin name t)) = liftM (ExpressionBuiltin . Builtin name) t
sequenceExpression (ExpressionGetVariable var) = return $ ExpressionGetVariable var
sequenceExpression ExpressionHole = return ExpressionHole
sequenceExpression (ExpressionLiteralInteger int) = return $ ExpressionLiteralInteger int
sequenceExpression ExpressionMagic = return ExpressionMagic

mapMExpression
  :: Monad m
  => (from
      -> ( m (Expression from)
         , Expression to -> m to ))
  -> from -> m to
mapMExpression f src =
  afterRecurse =<< sequenceExpression . mapExpression (mapMExpression f) =<< makeExpr
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
  sequenceExpression $ mapExpression (newIRefExpressionFromPureH newScope) newExpr
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
      liftM (PureGuidExpression . GuidExpression newGuid) $
        case v of
        ExpressionLambda lambda ->
          liftM ExpressionLambda $ onLambda oldGuid newGuid lambda
        ExpressionPi lambda ->
          liftM ExpressionPi $ onLambda oldGuid newGuid lambda
        ExpressionApply (Apply func arg) ->
          liftM ExpressionApply $
          liftM2 Apply (go func) (go arg)
        gv@(ExpressionGetVariable (ParameterRef guid)) ->
          Reader.asks $
          maybe gv (ExpressionGetVariable . ParameterRef) .
          Map.lookup guid
        x -> return x
