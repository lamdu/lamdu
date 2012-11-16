{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedStrings,
             DeriveFunctor, DeriveFoldable, DeriveTraversable, ConstraintKinds #-}
module Editor.CodeEdit.Sugar
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
  , ExpressionBody(..)
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..), rGuid, rExpressionBody, rPayload
  , Expression
  , WhereItem(..)
  , Func(..)
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), HoleActions(..), HoleResult, holeResultHasHoles
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  , loadConvertDefinition, loadConvertExpression
  , removeTypes
  ) where

import Control.Applicative ((<$>), (<$), Applicative(..))
import Control.Arrow (first, (&&&))
import Control.Lens ((^.))
import Control.Monad ((<=<), liftM, mplus, void, zipWithM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Binary (Binary(..))
import Data.Cache (Cache)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Function (on)
import Data.Hashable (hash)
import Data.List.Utils (sortOn)
import Data.Map (Map)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Monoid(..), Any(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.Sugar.Config (SugarConfig)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Cache as Cache
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer
import qualified Editor.Data.Load as Load
import qualified Editor.Data.Ops as DataOps
import qualified System.Random as Random
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

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRef
  :: Monad m => DataIRef.ExpressionProperty (T m)
  -> Data.ExpressionBody Data.ExpressionIRef
  -> Transaction t m ()
writeIRef = DataIRef.writeExprBody . Property.value

writeIRefVia
  :: Monad m
  => (a -> DataIRef.ExpressionBody)
  -> DataIRef.ExpressionProperty (T m)
  -> a -> Transaction t m ()
writeIRefVia f = (fmap . argument) f writeIRef

data SugarContext = SugarContext
  { scInferState :: Infer.RefMap
  , scConfig :: SugarConfig
  }

newtype Sugar m a = Sugar (ReaderT SugarContext (T m) a)
  deriving (Monad)

runSugar :: Monad m => SugarContext -> Sugar m a -> T m a
runSugar ctx (Sugar action) = runReaderT action ctx

readContext :: Monad m => Sugar m SugarContext
readContext = Sugar Reader.ask

liftTransaction :: Monad m => T m a -> Sugar m a
liftTransaction = Sugar . lift

type Convertor m = ExprEntity m -> Sugar m (Expression m)

mkCutter :: Monad m => Data.ExpressionIRef -> T m Guid -> T m Guid
mkCutter iref replaceWithHole = do
  Anchors.modP Anchors.clipboards (iref:)
  replaceWithHole

mkActions :: Monad m => DataIRef.ExpressionProperty (T m) -> Actions m
mkActions stored =
  Actions
  { giveAsArg = guidify $ DataOps.giveAsArg stored
  , replace = doReplace
  , cut = mkCutter (Property.value stored) doReplace
  , giveAsArgToOperator = guidify . DataOps.giveAsArgToOperator stored
  }
  where
    guidify = liftM DataIRef.exprGuid
    doReplace = guidify $ DataOps.replaceWithHole stored

mkGen :: Int -> Int -> Guid -> Random.StdGen
mkGen select count =
  Random.mkStdGen . (+select) . (*count) . BinaryUtils.decodeS . Guid.bs

mkExpression ::
  Monad m =>
  ExprEntity m ->
  ExpressionBody m (Expression m) -> Sugar m (Expression m)
mkExpression ee expr = do
  inferredTypesRefs <-
    zipWithM (fmap convertExpressionI . eeFromPure) seeds types
  return
    Expression
    { _rGuid = eeGuid ee
    , _rExpressionBody = expr
    , _rPayload = Payload
      { _plInferredTypes = inferredTypesRefs
      , _plActions = mkActions <$> eeProp ee
      , _plNextHole = Nothing
      }
    }
  where
    seeds = RandomUtils.splits . mkGen 0 2 $ eeGuid ee
    types = maybe [] eeiInferredTypes . eplInferred $ ee ^. Data.ePayload

mkDelete
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> DataIRef.ExpressionProperty (T m)
  -> T m Guid
mkDelete parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

mkAddParam ::
  Monad m => DataIRef.ExpressionProperty (T m) -> T m Guid
mkAddParam = liftM fst . DataOps.lambdaWrap

storedIRefP :: Data.Expression (ExprEntityInferred a) -> a
storedIRefP = Infer.iStored . eeiInferred . Lens.view Data.ePayload

mkFuncParamActions ::
  Monad m => SugarContext ->
  ExprEntityStored m ->
  Data.Lambda (ExprEntityStored m) ->
  DataIRef.ExpressionProperty (T m) ->
  FuncParamActions m
mkFuncParamActions
  ctx lambdaStored (Data.Lambda param paramType _) replacerP =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = mkDelete (Infer.iStored lambdaInferred) replacerP
    , _itemAddNext = mkAddParam replacerP
    }
  , _fpGetExample = do
      exampleP <-
        lift . Anchors.nonEmptyAssocDataRef "example" param .
        DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole
      exampleLoaded <- lift $ Load.loadExpressionProperty exampleP

      (_, inferState, exampleStored) <-
        inferLoadedExpression Nothing exampleLoaded newInferNode
      lift . convertStoredExpression exampleStored $
        SugarContext inferState (scConfig ctx)
  }
  where
    lambdaInferred = eeiInferred lambdaStored
    scope = Infer.nScope $ Infer.iPoint lambdaInferred
    paramTypeRef =
      Infer.tvVal . Infer.nRefs . Infer.iPoint $ eeiInferred paramType
    newInferNode =
      Infer.newTypedNodeWithScope scope paramTypeRef $ scInferState ctx

convertLambda
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> ExprEntity m -> Sugar m (FuncParam m (Expression m), Expression m)
convertLambda lam@(Data.Lambda param paramTypeI bodyI) expr = do
  sBody <- convertExpressionI bodyI
  typeExpr <- convertExpressionI paramTypeI
  ctx <- readContext
  let
    fp = FuncParam
      { _fpGuid = param
      , _fpHiddenLambdaGuid = Nothing
      , _fpType = removeRedundantTypes typeExpr
      , _fpMActions =
        mkFuncParamActions ctx
        <$> eeStored expr
        <*> Traversable.mapM eeStored lam
        <*> eeProp bodyI
      }
  return (fp, sBody)

convertFunc
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertFunc lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI .
    ExpressionFunc DontHaveParens $
    case sBody ^. rExpressionBody of
      ExpressionFunc _ (Func nextParams body) ->
        case nextParams of
        [] -> error "Func must have at least 1 param!"
        (nextParam : _) ->
          Func (deleteToNextParam nextParam param : nextParams) body
      _ -> Func [param] sBody
  where
    deleteToNextParam nextParam =
      Lens.set (fpMActions . Lens.mapped . fpListItemActions .  itemDelete . Lens.sets liftM) $ nextParam ^. fpGuid

convertPi
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertPi lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = Lens.over fpType addApplyChildParens param
    , pResultType = removeRedundantTypes sBody
    }

addParens :: ExpressionBody m (Expression m) -> ExpressionBody m (Expression m)
addParens (ExpressionInferred (Inferred val hole)) =
  ExpressionInferred $ Inferred (Lens.over rExpressionBody addParens val) hole
addParens (ExpressionPolymorphic (Polymorphic g compact full)) =
  ExpressionPolymorphic . Polymorphic g compact $
  Lens.over rExpressionBody addParens full
addParens x = Lens.set eHasParens HaveParens x

addApplyChildParens :: Expression m -> Expression m
addApplyChildParens =
  Lens.over rExpressionBody f
  where
    f x@ExpressionApply{} = x
    f x@ExpressionPolymorphic{} = x
    f x = addParens x

isPolymorphicFunc :: ExprEntity m -> Bool
isPolymorphicFunc funcI =
  maybe False
  (Data.isDependentPi . Infer.iType . eeiInferred) .
  eplInferred $ funcI ^. Data.ePayload

convertApply :: Monad m => Data.Apply (ExprEntity m) -> Convertor m
convertApply (Data.Apply funcI argI) exprI = do
  funcS <- convertExpressionI funcI
  argS <- convertExpressionI argI
  let apply = Data.Apply (funcS, funcI) (argS, argI)
  case funcS ^. rExpressionBody of
    ExpressionSection _ section ->
      applyOnSection section apply exprI
    _ ->
      convertApplyPrefix apply exprI

removeInferredTypes :: Expression m -> Expression m
removeInferredTypes = Lens.set (rPayload . plInferredTypes) []

removeRedundantTypes :: Expression m -> Expression m
removeRedundantTypes =
  Lens.over (rPayload . plInferredTypes) removeIfNoErrors
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

isSameOp :: ExpressionBody m expr -> ExpressionBody m expr -> Bool
isSameOp (ExpressionPolymorphic p0) (ExpressionPolymorphic p1) =
  on (==) pCompact p0 p1
isSameOp (ExpressionGetVariable v0) (ExpressionGetVariable v1) =
  v0 == v1
isSameOp _ _ = False

setNextHole :: Expression m -> Expression m -> Expression m
setNextHole possibleHole =
  case possibleHole ^. rExpressionBody of
  ExpressionHole{} ->
    (fmap . Lens.over plNextHole . flip mplus . Just) possibleHole
  _ -> id

applyOnSection ::
  Monad m =>
  Section (Expression m) -> Data.Apply (Expression m, ExprEntity m) -> Convertor m
applyOnSection (Section Nothing op Nothing) (Data.Apply (_, funcI) arg@(argRef, _)) exprI
  | isPolymorphicFunc funcI = do
    newOpRef <-
      convertApplyPrefix (Data.Apply (op, funcI) arg) exprI
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeRedundantTypes newOpRef) Nothing
  | otherwise =
    mkExpression exprI . ExpressionSection DontHaveParens $
    Section (Just (addApplyChildParens argRef)) op Nothing
applyOnSection (Section (Just left) op Nothing) (Data.Apply _ (argRef, _)) exprI =
  mkExpression exprI . ExpressionSection DontHaveParens $
  on (Section . Just) (setNextHole right) left op (Just right)
  where
    right =
      case argRef ^. rExpressionBody of
      ExpressionSection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (Lens.view rExpressionBody) op rightOp -> argRef
      _ -> addApplyChildParens argRef
applyOnSection _ apply exprI = convertApplyPrefix apply exprI

convertApplyPrefix ::
  Monad m =>
  Data.Apply (Expression m, ExprEntity m) -> Convertor m
convertApplyPrefix (Data.Apply (funcRef, funcI) (argRef, _)) exprI
  | isPolymorphicFunc funcI =
    case funcRef ^. rExpressionBody of
    ExpressionPolymorphic (Polymorphic g compact full) ->
      makePolymorphic g compact =<< makeApply full
    ExpressionGetVariable getVar ->
      makePolymorphic (eeGuid funcI) getVar =<< makeFullApply
    _ -> makeFullApply
  | otherwise = makeFullApply
  where
    newArgRef = Lens.over rExpressionBody addParens argRef
    newFuncRef =
      setNextHole newArgRef .
      addApplyChildParens .
      removeRedundantTypes $
      funcRef
    expandedGuid = Guid.combine (eeGuid exprI) $ Guid.fromString "polyExpanded"
    makeFullApply = makeApply newFuncRef
    makeApply f =
      mkExpression exprI . ExpressionApply DontHaveParens $
      Data.Apply f newArgRef
    makePolymorphic g compact fullExpression =
      mkExpression exprI $ ExpressionPolymorphic Polymorphic
        { pFuncGuid = g
        , pCompact = compact
        , pFullExpression =
          Lens.set rGuid expandedGuid $ removeInferredTypes fullExpression
        }


isHole :: Data.ExpressionBody a -> Bool
isHole (Data.ExpressionLeaf Data.Hole) = True
isHole _ = False

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- liftTransaction $ Infix.isInfixVar varRef
  getVarExpr <-
    mkExpression exprI $ ExpressionGetVariable varRef
  if isInfix
    then
      mkExpression exprI .
      ExpressionSection HaveParens $
      Section Nothing (removeInferredTypes getVarExpr) Nothing
    else return getVarExpr

mkPaste :: Monad m => DataIRef.ExpressionProperty (T m) -> Sugar m (Maybe (T m Guid))
mkPaste exprP = do
  clipboardsP <- liftTransaction Anchors.clipboards
  let
    mClipPop =
      case Property.value clipboardsP of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set clipboardsP clips)
  return $ fmap (doPaste (Property.set exprP)) mClipPop
  where
    doPaste replacer (clip, popClip) = do
      ~() <- popClip
      ~() <- replacer clip
      return $ DataIRef.exprGuid clip

pureHole :: Data.PureExpression
pureHole = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

countArrows :: Data.PureExpression -> Int
countArrows Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  } = 1 + countArrows resultType
countArrows _ = 0

-- TODO: Return a record, not a tuple
countPis :: Data.PureExpression -> (Int, Int)
countPis e@Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  }
  | Data.isDependentPi e = first (1+) $ countPis resultType
  | otherwise = (0, 1 + countArrows resultType)
countPis _ = (0, 0)

applyForms
  :: Data.PureExpression
  -> Data.PureExpression -> [Data.PureExpression]
applyForms _ e@Data.Expression{ Data._eValue = Data.ExpressionLambda {} } =
  [e]
applyForms exprType expr =
  reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = iterate addApply expr !! depPis
    (depPis, arrows) = countPis exprType
    addApply = Data.pureExpression . (`Data.makeApply` pureHole)

convertReadOnlyHole :: Monad m => Convertor m
convertReadOnlyHole exprI =
  mkExpression exprI $ ExpressionHole Hole
  { holeScope = []
  , holeInferResults = const $ return []
  , holeMActions = Nothing
  }

loader :: Monad m => Infer.Loader (T m)
loader =
  Infer.Loader
  (liftM void . Load.loadExpressionIRef . Data.defType <=<
   Transaction.readIRef)

-- Fill partial holes in an expression. Parital holes are those whose
-- inferred (filler) value itself is not complete, so will not be a
-- useful auto-inferred value. By auto-filling those, we allow the
-- user a chance to access all the partiality that needs filling more
-- easily.
fillPartialHolesInExpression ::
  Monad m =>
  (Data.PureExpression -> m (Maybe (Infer.Expression a))) ->
  Infer.Expression a -> m [Infer.Expression a]
fillPartialHolesInExpression check oldExpr =
  liftM ((++ [oldExpr]) . maybeToList) .
  recheck . runWriter $ fillHoleExpr oldExpr
  where
    recheck (newExpr, Any True) = check newExpr
    recheck (_, Any False) = return Nothing
    fillHoleExpr expr@(Data.Expression (Data.ExpressionLeaf Data.Hole) hInferred) =
      let inferredVal = Infer.iValue hInferred
      in
        case inferredVal ^. Data.eValue of
        Data.ExpressionLeaf Data.Hole -> return $ void expr
        _ | isCompleteType inferredVal -> return $ void expr
          | otherwise -> do
            -- Hole inferred value has holes to fill, no use leaving it as
            -- auto-inferred, just fill it:
            Writer.tell $ Any True
            return inferredVal
    fillHoleExpr (Data.Expression body _) =
      liftM Data.pureExpression $ Traversable.mapM fillHoleExpr body

resultComplexityScore :: HoleResult -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

convertWritableHole :: Monad m => ExprEntityStored m -> Convertor m
convertWritableHole eeInferred exprI = do
  ctx <- readContext
  mPaste <- mkPaste . Infer.iStored $ eeiInferred eeInferred
  convertWritableHoleH ctx mPaste eeInferred exprI

inferApplyForms ::
  Monad m =>
  (Data.PureExpression -> CT m [HoleResult]) -> Data.PureExpression ->
  (Infer.RefMap, Infer.InferNode) -> CT m [HoleResult]
inferApplyForms processRes expr (refMap, node) =
  liftM (sortOn resultComplexityScore) . makeApplyForms =<<
  inferExpr expr refMap node
  where
    makeApplyForms Nothing = return []
    makeApplyForms (Just i) =
      liftM concat . mapM processRes $
      applyForms (Infer.iType (i ^. Data.ePayload)) expr

convertWritableHoleH ::
  Monad m =>
  SugarContext -> Maybe (T m Guid) ->
  ExprEntityStored m -> Convertor m
convertWritableHoleH (SugarContext inferState config) mPaste eeInferred exprI =
  chooseHoleType (eeiInferredValues eeInferred) plainHole inferredHole
  where
    inferred = eeiInferred eeInferred
    scope = Infer.nScope $ Infer.iPoint inferred
    check expr = inferExpr expr inferState $ Infer.iPoint inferred

    inferResults processRes expr =
      inferApplyForms processRes expr $
      Infer.newNodeWithScope scope inferState
    onScopeElement (param, _typeExpr) = param
    mkHole processRes = Hole
      { holeScope =
        map onScopeElement . Map.toList $ Infer.iScope inferred
      , holeInferResults = inferResults processRes
      , holeMActions = Just HoleActions
          { holePickResult = pickResult eGuid $ Infer.iStored inferred
          , holePaste = mPaste
          , holeConvertResult = convertHoleResult config
          }
      }
    filledHole =
      mkHole (maybe (return []) (fillPartialHolesInExpression check) <=< check)
    inferredHole inferredVal =
      mkExpression exprI .
      ExpressionInferred . (`Inferred` filledHole) =<<
      convertExpressionI
      (eeFromPure (mkGen 1 2 eGuid) inferredVal)
    plainHole =
      wrapOperatorHole exprI <=<
      mkExpression exprI . ExpressionHole $ mkHole (liftM maybeToList . check)
    eGuid = eeGuid exprI

wrapOperatorHole ::
  Monad m => ExprEntity m -> Expression m -> Sugar m (Expression m)
wrapOperatorHole exprI holeExpr = do
  searchTermRef <- liftTransaction . Anchors.assocSearchTermRef $ eeGuid exprI
  if isOperatorName $ Property.value searchTermRef
    then
      -- TODO: Ok to mkExpression with same exprI here?
      mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeInferredTypes holeExpr) Nothing
    else return holeExpr

isOperatorName :: String -> Bool
isOperatorName name =
  not (null name) && all (`elem` Config.operatorChars) name

inferExpr ::
  (Monad m, Cache.Key a) => Data.Expression a -> Infer.RefMap ->
  Infer.InferNode -> CT m (Maybe (Infer.Expression a))
inferExpr expr inferContext inferPoint = do
  loaded <- lift $ Infer.load loader Nothing expr
  Cache.memoS (return . fmap fst . uncurriedInfer)
    (loaded, inferContext, inferPoint)
  where
    uncurriedInfer (loaded, ctx, pt) =
      Infer.infer (Infer.InferActions (const Nothing))
      loaded ctx pt

chooseHoleType ::
  [Data.Expression f] -> hole -> (Data.Expression f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Data.Expression { Data._eValue = Data.ExpressionLeaf Data.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  (Monad f, Monad m) =>
  Guid -> DataIRef.ExpressionProperty (T m) ->
  Data.Expression (Infer.Inferred a) ->
  Transaction t f (Guid, Actions m)
pickResult defaultDest irefP =
  liftM
  ( flip (,) (mkActions irefP)
  . maybe defaultDest
    (DataIRef.exprGuid . Infer.iStored . Lens.view Data.ePayload)
  . listToMaybe . uninferredHoles . fmap intoStored
  )
  . DataIRef.writeExpression (Property.value irefP)
  where
    intoStored (exprIRef, inferred) = (fmap . const) exprIRef inferred

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles :: Infer.Expression a -> [Infer.Expression a]
uninferredHoles Data.Expression { Data._eValue = Data.ExpressionApply (Data.Apply func arg) } =
  if (Data.isDependentPi . Infer.iType . Lens.view Data.ePayload) func
  then uninferredHoles func
  else uninferredHoles func ++ uninferredHoles arg
uninferredHoles e@Data.Expression { Data._eValue = Data.ExpressionLeaf Data.Hole } = [e]
uninferredHoles Data.Expression
  { Data._eValue = Data.ExpressionPi (Data.Lambda _ paramType resultType) } =
    uninferredHoles resultType ++ uninferredHoles paramType
uninferredHoles Data.Expression
  { Data._eValue = Data.ExpressionLambda (Data.Lambda _ paramType result) } =
    uninferredHoles result ++ uninferredHoles paramType
uninferredHoles Data.Expression { Data._eValue = body } =
  Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult -> Bool
holeResultHasHoles = not . null . uninferredHoles

convertHole :: Monad m => Convertor m
convertHole exprI =
  maybe convertReadOnlyHole convertWritableHole mStored exprI
  where
    mStored = f =<< eplInferred (exprI ^. Data.ePayload)
    f entity = fmap (g entity) $ (Infer.iStored . eeiInferred) entity
    g entity stored =
      (atEeiInferred . fmap . const) stored entity
    atEeiInferred j x = x { eeiInferred = j $ eeiInferred x }

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpression exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue =
      fmap (writeIRefVia (Data.ExpressionLeaf . Data.LiteralInteger)) $
      eeProp exprI
  }

convertAtom :: Monad m => String -> Convertor m
convertAtom name exprI =
  mkExpression exprI $ ExpressionAtom name

convertExpressionI :: Monad m => ExprEntity m -> Sugar m (Expression m)
convertExpressionI ee =
  ($ ee) $
  case ee ^. Data.eValue of
  Data.ExpressionLambda x -> convertFunc x
  Data.ExpressionPi x -> convertPi x
  Data.ExpressionApply x -> convertApply x
  Data.ExpressionLeaf (Data.GetVariable x) -> convertGetVariable x
  Data.ExpressionLeaf (Data.LiteralInteger x) -> convertLiteralInteger x
  Data.ExpressionLeaf Data.Hole -> convertHole
  Data.ExpressionLeaf Data.Set -> convertAtom "Set"
  Data.ExpressionLeaf Data.IntegerType -> convertAtom "Int"

-- Check no holes
isCompleteType :: Data.PureExpression -> Bool
isCompleteType = not . any (isHole . Lens.view Data.eValue) . Data.subExpressions

runPureSugar :: Monad m => SugarConfig -> Sugar m a -> T m a
runPureSugar config =
  runSugar ctx
  where
    ctx =
      SugarContext
      { scInferState = error "pure expression doesnt have infer state"
      , scConfig = config
      }

convertHoleResult ::
  Monad m => SugarConfig -> HoleResult -> T m (Expression m)
convertHoleResult config holeResult =
  runPureSugar config . convertExpressionI . Data.randomizeExpr gen $
  fmap toExprEntity holeResult
  where
    gen = Random.mkStdGen . hash . show $ void holeResult
    toExprEntity inferred =
      flip EntityPayload $
      Just ExprEntityInferred
      { eeiInferred = (fmap . const) Nothing inferred
      , eeiTypeConflicts = []
      , eeiValueConflicts = []
      }

convertExpressionPure ::
  (Monad m, RandomGen g) => g -> SugarConfig -> Data.PureExpression -> T m (Expression m)
convertExpressionPure gen config =
  runPureSugar config . convertExpressionI . eeFromPure gen

convertDefinitionParams ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m ([FuncParam m (Expression m)], Data.Expression (ExprEntityStored m))
convertDefinitionParams ctx expr =
  case expr ^. Data.eValue of
  Data.ExpressionLambda lam@(Data.Lambda param paramType body) -> do
    paramTypeS <- convertStoredExpression paramType ctx
    let
      fp = FuncParam
        { _fpGuid = param
        , _fpHiddenLambdaGuid = Just . eeiGuid $ expr ^. Data.ePayload
        , _fpType = removeRedundantTypes paramTypeS
        , _fpMActions =
          Just $
          mkFuncParamActions ctx
          (expr ^. Data.ePayload) (fmap (Lens.view Data.ePayload) lam)
          (storedIRefP body)
        }
    (nextFPs, funcBody) <- convertDefinitionParams ctx body
    return (fp : nextFPs, funcBody)
  _ -> return ([], expr)

convertWhereItems ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m ([WhereItem m], Data.Expression (ExprEntityStored m))
convertWhereItems ctx
  topLevel@Data.Expression
  { Data._eValue = Data.ExpressionApply apply@Data.Apply
  { Data._applyFunc = Data.Expression
  { Data._eValue = Data.ExpressionLambda lambda@Data.Lambda
  { Data._lambdaParamId = param
  , Data._lambdaParamType = Data.Expression
  { Data._eValue = Data.ExpressionLeaf Data.Hole
  }}}}} = do
    value <- convertDefinitionContent ctx $ apply ^. Data.applyArg
    let
      body = lambda ^. Data.lambdaBody
      item = WhereItem
        { wiValue = value
        , wiGuid = param
        , wiHiddenGuids =
            map (eeiGuid . Lens.view Data.ePayload)
            [ topLevel
            , lambda ^. Data.lambdaParamType
            ]
        , wiActions =
            ListItemActions
            { _itemDelete = mkDelete (prop topLevel) (prop body)
            , _itemAddNext = liftM fst . DataOps.redexWrap $ prop topLevel
            }
        }
    (nextItems, whereBody) <- convertWhereItems ctx body
    return (item : nextItems, whereBody)
  where
    prop = Infer.iStored . eeiInferred . Lens.view Data.ePayload
convertWhereItems _ expr = return ([], expr)

convertDefinitionContent ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m (DefinitionContent m)
convertDefinitionContent sugarContext expr = do
  (params, funcBody) <- convertDefinitionParams sugarContext expr
  (whereItems, whereBody) <- convertWhereItems sugarContext funcBody
  bodyS <- convertStoredExpression whereBody sugarContext
  return DefinitionContent
    { dBody = bodyS
    , dParameters = params
    , dWhereItems = whereItems
    , dAddFirstParam = mkAddParam $ stored expr
    , dAddInnermostWhereItem =
        liftM fst . DataOps.redexWrap $ stored whereBody
    }
  where
    stored = Infer.iStored . eeiInferred . Lens.view Data.ePayload

loadConvertDefinition ::
  Monad m => SugarConfig -> Data.DefinitionIRef ->
  CT m (Definition m)
loadConvertDefinition config defI =
  -- TODO: defI given twice probably means the result of
  -- loadDefinition is missing some defI-dependent values
  convertDefinition config defI =<<
  lift (Load.loadDefinition defI)

convertDefinitionBuiltin ::
  Monad m =>
  Data.Builtin -> Data.DefinitionIRef ->
  Load.Loaded (T m) ->
  DefinitionBody m
convertDefinitionBuiltin (Data.Builtin name) defI (Load.Stored _ typeIRef) =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeI = typeIRef ^. Data.ePayload
    setName =
      Transaction.writeIRef defI . (`Data.Definition` typeI) .
      Data.DefinitionBuiltin . Data.Builtin

convertDefinitionExpression ::
  Monad m => SugarConfig ->
  Load.Loaded (T m) ->
  Data.DefinitionIRef ->
  Load.Loaded (T m) ->
  CT m (DefinitionBody m)
convertDefinitionExpression config exprLoaded defI (Load.Stored setType typeIRef) = do
  (isSuccess, inferState, exprStored) <-
    inferLoadedExpression (Just defI) exprLoaded Infer.initial
  let
    inferredTypeP =
      Infer.iType . eeiInferred $ exprStored ^. Data.ePayload
    typesMatch = on (==) Data.canonizeParamIds (void typeIRef) inferredTypeP
    mkNewType = do
      inferredTypeS <-
        convertExpressionPure (mkGen 0 2 (IRef.guid defI)) config
        inferredTypeP
      return DefinitionNewType
        { dntNewType = inferredTypeS
        , dntAcceptNewType =
          setType =<< DataIRef.newExpression inferredTypeP
        }
    sugarContext =
      SugarContext
      { scInferState = inferState
      , scConfig = config
      }
  content <- lift $ convertDefinitionContent sugarContext exprStored
  mNewType <- lift $
    if isSuccess && not typesMatch && isCompleteType inferredTypeP
    then liftM Just mkNewType
    else return Nothing
  return $ DefinitionBodyExpression DefinitionExpression
    { deContent = content
    , deMNewType = mNewType
    , deIsTypeRedundant = isSuccess && typesMatch
    }

convertDefinition ::
  Monad m => SugarConfig ->
  Data.DefinitionIRef ->
  Data.Definition (Load.Loaded (T m)) ->
  CT m (Definition m)
convertDefinition config defI def = do
  body <- convertDefBody defBody defI typeLoaded
  typeS <-
    lift .
    convertExpressionPure (mkGen 1 2 (IRef.guid defI)) config .
    void $ Load.sExpr typeLoaded
  return Definition
    { drGuid = IRef.guid defI
    , drBody = body
    , drType = typeS
    }
  where
    Data.Definition defBody typeLoaded = def
    convertDefBody (Data.DefinitionBuiltin builtin) =
      fmap return . convertDefinitionBuiltin builtin
    convertDefBody (Data.DefinitionExpression exprLoaded) =
      convertDefinitionExpression config exprLoaded

inferredIRefToStored ::
  Monad m => Load.ExpressionSetter (T m) ->
  Data.Expression (ExprEntityInferred Data.ExpressionIRef) ->
  Data.Expression (ExprEntityStored m)
inferredIRefToStored setter expr =
  fmap propIntoInferred . Load.exprAddProp . Load.Stored setter $
  fmap (Infer.iStored . eeiInferred &&& id) expr
  where
    propIntoInferred (prop, eei) = prop <$ eei

third :: (c0 -> c1) -> (a, b, c0) -> (a, b, c1)
third f (x, y, z) = (x, y, f z)

inferLoadedExpression ::
  Monad m =>
  Maybe Data.DefinitionIRef -> Load.Loaded (T m) ->
  (Infer.RefMap, Infer.InferNode) ->
  CT m
   (Bool, Infer.RefMap,
    Data.Expression (ExprEntityStored m))
inferLoadedExpression mDefI (Load.Stored setExpr exprIRef) inferState = do
  loaded <- lift $ Infer.load loader mDefI exprIRef
  (liftM . third) (inferredIRefToStored setExpr) $
    Cache.memoS (return . uncurriedInfer) (loaded, inferState)
  where
    uncurriedInfer (loaded, (refMap, inferNode)) =
      inferWithConflicts loaded refMap inferNode

-- Conflicts:

newtype ConflictMap =
  ConflictMap { unConflictMap :: Map Infer.Ref (Set Data.PureExpression) }

instance Monoid ConflictMap where
  mempty = ConflictMap mempty
  mappend (ConflictMap x) (ConflictMap y) =
    ConflictMap $ Map.unionWith mappend x y

getConflicts :: Infer.Ref -> ConflictMap -> [Data.PureExpression]
getConflicts ref = maybe [] Set.toList . Map.lookup ref . unConflictMap

reportConflict :: Infer.Error -> Writer ConflictMap ()
reportConflict err =
  Writer.tell . ConflictMap .
  Map.singleton (Infer.errRef err) .
  Set.singleton .
  snd $ Infer.errMismatch err

inferWithConflicts ::
  Infer.Loaded a ->
  Infer.RefMap -> Infer.InferNode ->
  ( Bool
  , Infer.RefMap
  , Data.Expression (ExprEntityInferred a)
  )
inferWithConflicts loaded refMap node =
  ( Map.null $ unConflictMap conflictsMap
  , inferContext
  , fmap toExprEntity exprInferred
  )
  where
    ((exprInferred, inferContext), conflictsMap) =
      runWriter $ Infer.infer (Infer.InferActions reportConflict)
      loaded refMap node
    toExprEntity x =
      ExprEntityInferred
      { eeiInferred = x
      , eeiValueConflicts = conflicts Infer.tvVal x
      , eeiTypeConflicts = conflicts Infer.tvType x
      }
    conflicts getRef x =
      getConflicts ((getRef . Infer.nRefs . Infer.iPoint) x)
      conflictsMap

--------------

loadConvertExpression ::
  Monad m =>
  SugarConfig ->
  DataIRef.ExpressionProperty (T m) ->
  CT m (Expression m)
loadConvertExpression config exprP =
  convertLoadedExpression =<< lift (Load.loadExpressionProperty exprP)
  where
    convertLoadedExpression exprLoaded = do
      (_, inferState, exprStored) <- inferLoadedExpression Nothing exprLoaded Infer.initial
      lift . convertStoredExpression exprStored $ SugarContext inferState config

convertStoredExpression ::
  Monad m =>
  Data.Expression (ExprEntityStored m) -> SugarContext ->
  T m (Expression m)
convertStoredExpression expr sugarContext =
  runSugar sugarContext . convertExpressionI $
  fmap f expr
  where
    f i = EntityPayload
      { eplGuid = eeiGuid i
      , eplInferred = Just $ fmap Just i
      }

removeTypes :: Expression m -> Expression m
removeTypes = removeInferredTypes . (Lens.over rExpressionBody . fmap) removeTypes

eeiInferredExprs ::
  (Infer.Inferred a -> b) ->
  (ExprEntityInferred a -> [b]) ->
  ExprEntityInferred a -> [b]
eeiInferredExprs getVal eeConflicts ee =
  getVal (eeiInferred ee) : eeConflicts ee

eeiInferredTypes :: ExprEntityInferred a -> [Data.PureExpression]
eeiInferredTypes = eeiInferredExprs Infer.iType eeiTypeConflicts

eeiInferredValues :: ExprEntityInferred a -> [Data.PureExpression]
eeiInferredValues = eeiInferredExprs Infer.iValue eeiValueConflicts
