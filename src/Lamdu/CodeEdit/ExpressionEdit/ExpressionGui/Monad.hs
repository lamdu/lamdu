{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad
  ( ExprGuiM, WidgetT, run
  , widgetEnv

  , transaction, atEnv, withFgColor
  , getP, assignCursor, assignCursorPrefix
  , wrapDelegated
  --
  , makeSubexpresion
  --
  , readSettings
  --
  , AccessedVars, markVariablesAsUsed, usedVariables
  , withParamName, NameSource(..)
  , withNameFromVarRef
  , getDefName
  , memo, memoT
  , fmapemo, fmapemoT
  , unmemo
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.Monad.Trans.State (StateT(..), evalStateT, mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Map (Map)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Anchors (ViewM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Types (ExpressionGui, WidgetT)
import Lamdu.CodeEdit.Settings (Settings)
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Cache as Cache
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef
import qualified Lamdu.WidgetEnvT as WE

type AccessedVars = [Guid]

data NameGenState = NameGenState
  { ngUnusedNames :: [String]
  , ngUsedNames :: Map Guid String
  }

data Askable m = Askable
  { _aNameGenState :: NameGenState
  , _aSettings :: Settings
  , _aMakeSubexpression :: Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
  }

type T = Transaction

newtype ExprGuiM m a = ExprGuiM
  { _varAccess :: RWST (Askable m) AccessedVars Cache (WidgetEnvT (T m)) a
  }
  deriving (Functor, Applicative, Monad)

LensTH.makeLenses ''Askable
LensTH.makeLenses ''ExprGuiM

atEnv :: MonadA m => (WE.Env -> WE.Env) -> ExprGuiM m a -> ExprGuiM m a
atEnv = Lens.over varAccess . RWS.mapRWST . WE.atEnv

withFgColor :: MonadA m => Draw.Color -> ExprGuiM m a -> ExprGuiM m a
withFgColor = atEnv . WE.setTextColor

readSettings :: MonadA m => ExprGuiM m Settings
readSettings = ExprGuiM . RWS.asks $ Lens.view aSettings

makeSubexpresion :: MonadA m => Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
makeSubexpresion expr = do
  maker <- ExprGuiM . RWS.asks $ Lens.view aMakeSubexpression
  maker expr

fmapemo :: MonadA m => StateT Cache (WidgetEnvT (T m)) a -> ExprGuiM m a
fmapemo act = ExprGuiM $ do
  cache <- RWS.get
  (val, newCache) <- lift $ runStateT act cache
  RWS.put newCache
  return val

fmapemoT :: MonadA m => StateT Cache (T m) a -> ExprGuiM m a
fmapemoT = fmapemo . mapStateT lift

memo ::
  (Cache.Key k, Binary v, MonadA m) =>
  (k -> WidgetEnvT (T m) v) -> k -> ExprGuiM m v
memo f key = fmapemo $ Cache.memoS f key

memoT ::
  (Cache.Key k, Binary v, MonadA m) =>
  (k -> T m v) -> k -> ExprGuiM m v
memoT f = memo (lift . f)

unmemo :: MonadA m => StateT Cache m a -> m a
unmemo = (`evalStateT` Cache.new 0)

run ::
  MonadA m =>
  (Sugar.Expression m -> ExprGuiM m (ExpressionGui m)) ->
  Settings -> ExprGuiM m a -> StateT Cache (WidgetEnvT (T m)) a
run makeSubexpression settings (ExprGuiM action) = StateT $ \cache ->
  fmap f $ runRWST action
  (Askable initialNameGenState settings makeSubexpression) cache
  where
    f (x, newCache, _) = (x, newCache)

widgetEnv :: MonadA m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv = ExprGuiM . lift

transaction :: MonadA m => T m a -> ExprGuiM m a
transaction = widgetEnv . lift

getP :: m ~ ViewM => Anchors.MkProperty ViewM a -> ExprGuiM m a
getP = transaction . Anchors.getP

assignCursor :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursor x y = atEnv $ WE.envAssignCursor x y

assignCursorPrefix :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursorPrefix x y = atEnv $ WE.envAssignCursorPrefix x y

wrapDelegated ::
  (MonadA f, MonadA m) =>
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  ((Widget f -> Widget f) -> a -> b) ->
  (Widget.Id -> ExprGuiM m a) ->
  Widget.Id -> ExprGuiM m b
wrapDelegated =
  BWidgets.wrapDelegatedWith (widgetEnv WE.readCursor)
  (atEnv . Lens.over WE.envCursor)

-- Used vars:

usedVariables
  :: MonadA m
  => ExprGuiM m a -> ExprGuiM m (a, [Guid])
usedVariables = Lens.over varAccess RWS.listen

markVariablesAsUsed :: MonadA m => AccessedVars -> ExprGuiM m ()
markVariablesAsUsed = ExprGuiM . RWS.tell

-- Auto-generating names

initialNameGenState :: NameGenState
initialNameGenState =
  NameGenState names Map.empty
  where
    alphabet = map (:[]) ['a'..'z']
    names = alphabet ++ liftA2 (++) names alphabet

withNewName :: MonadA m => Guid -> (String -> ExprGuiM m a) -> ExprGuiM m a
withNewName guid useNewName = do
  nameGen <- ExprGuiM . RWS.asks $ Lens.view aNameGenState
  let
    (name : nextNames) = ngUnusedNames nameGen
    newNameGen = nameGen
      { ngUnusedNames = nextNames
      , ngUsedNames = Map.insert guid name $ ngUsedNames nameGen
      }
  ExprGuiM .
    (RWS.local . Lens.set aNameGenState) newNameGen .
    Lens.view varAccess $ useNewName name

data NameSource = AutoGeneratedName | StoredName

withParamName :: MonadA m => Guid -> ((NameSource, String) -> ExprGuiM m a) -> ExprGuiM m a
withParamName guid useNewName = do
  storedName <- transaction . Anchors.getP $ Anchors.assocNameRef guid
  -- TODO: maybe use Maybe?
  if null storedName
    then do
      existingName <-
        ExprGuiM $ RWS.asks (Map.lookup guid . ngUsedNames . Lens.view aNameGenState)
      let useGenName = useNewName . (,) AutoGeneratedName
      case existingName of
        Nothing -> withNewName guid useGenName
        Just name -> useGenName name
    else useNewName (StoredName, storedName)

getDefName :: MonadA m => Guid -> ExprGuiM m (NameSource, String)
getDefName guid = do
  storedName <- transaction . Anchors.getP $ Anchors.assocNameRef guid
  return $
    if null storedName
    then (AutoGeneratedName, (("anon_"++) . take 6 . Guid.asHex) guid)
    else (StoredName, storedName)

withNameFromVarRef ::
  MonadA m => Data.VariableRef (DataIRef.DefI (Tag m)) ->
  ((NameSource, String) -> ExprGuiM m a) -> ExprGuiM m a
withNameFromVarRef (Data.ParameterRef g) useName = withParamName g useName
withNameFromVarRef (Data.DefinitionRef defI) useName =
  useName =<< getDefName (IRef.guid defI)
