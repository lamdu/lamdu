{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies, DeriveDataTypeable #-}
module Lamdu.GUI.ExpressionGui.Monad
  ( ExprGuiM, WidgetT
  , widgetEnv
  , StoredGuids(..), Injected(..)
  , HoleGuids(..), hgMNextHole, hgMPrevHole
  , emptyHoleGuids
  , Payload(..), plStoredGuids, plInjected, plHoleGuids
  , emptyPayload
  , SugarExpr

  , transaction, localEnv, withFgColor
  , getP, assignCursor, assignCursorPrefix
  , wrapDelegated
  --
  , makeSubexpression
  --
  , readSettings, readCodeAnchors
  , getCodeAnchor, mkPrejumpPosSaver
  --
  , addResultPicker, listenResultPickers

  , AccessedVars, markVariablesAsUsed, listenUsedVariables

  , memo, memoT
  , liftMemo, liftMemoT

  , run
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Types (ExpressionGui, WidgetT, ParentPrecedence(..), Precedence)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction
type AccessedVars = [Guid]

data Output m = Output
  { oAccessedVars :: AccessedVars
  , oHolePickers :: [T m ()]
  }
derive makeMonoid ''Output

newtype StoredGuids = StoredGuids [Guid]
  deriving (Monoid, Binary, Typeable, Eq, Ord)

newtype Injected = Injected [Bool]
  deriving (Monoid, Binary, Typeable, Eq, Ord)

data HoleGuids = HoleGuids
  { _hgMNextHole :: Maybe Guid
  , _hgMPrevHole :: Maybe Guid
  }
Lens.makeLenses ''HoleGuids

emptyHoleGuids :: HoleGuids
emptyHoleGuids = HoleGuids Nothing Nothing

-- GUI input payload on sugar exprs
data Payload = Payload
  { _plStoredGuids :: [Guid]
  , _plInjected :: [Bool]
  , _plHoleGuids :: HoleGuids
  }
Lens.makeLenses ''Payload

emptyPayload :: Payload
emptyPayload = Payload
  { _plStoredGuids = []
  , _plInjected = []
  , _plHoleGuids = emptyHoleGuids
  }

type SugarExpr m = Sugar.ExpressionN m Payload

data Askable m = Askable
  { _aSettings :: Settings
  , _aMakeSubexpression ::
    ParentPrecedence ->
    Sugar.ExpressionN m Payload ->
    ExprGuiM m (ExpressionGui m)
  , _aCodeAnchors :: Anchors.CodeProps m
  }

newtype GuiState = GuiState
  { _gsCache :: Cache
  }

newtype ExprGuiM m a = ExprGuiM
  { _exprGuiM :: RWST (Askable m) (Output m) GuiState (WidgetEnvT (T m)) a
  }
  deriving (Functor, Applicative, Monad)

Lens.makeLenses ''Askable
Lens.makeLenses ''ExprGuiM
Lens.makeLenses ''GuiState

-- TODO: To lens
localEnv :: MonadA m => (WE.Env -> WE.Env) -> ExprGuiM m a -> ExprGuiM m a
localEnv = (exprGuiM %~) . RWS.mapRWST . WE.localEnv

withFgColor :: MonadA m => Draw.Color -> ExprGuiM m a -> ExprGuiM m a
withFgColor = localEnv . WE.setTextColor

readSettings :: MonadA m => ExprGuiM m Settings
readSettings = ExprGuiM $ Lens.view aSettings

readCodeAnchors :: MonadA m => ExprGuiM m (Anchors.CodeProps m)
readCodeAnchors = ExprGuiM $ Lens.view aCodeAnchors

mkPrejumpPosSaver :: MonadA m => ExprGuiM m (T m ())
mkPrejumpPosSaver =
  DataOps.savePreJumpPosition <$> readCodeAnchors <*> widgetEnv WE.readCursor

-- TODO: makeSubexpresSion
makeSubexpression ::
  MonadA m => Precedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpression parentPrecedence expr = do
  maker <- ExprGuiM $ Lens.view aMakeSubexpression
  maker (ParentPrecedence parentPrecedence) expr

liftMemo :: MonadA m => StateT Cache (WidgetEnvT (T m)) a -> ExprGuiM m a
liftMemo act = ExprGuiM . Lens.zoom gsCache $ do
  cache <- RWS.get
  (val, newCache) <- lift $ runStateT act cache
  RWS.put newCache
  return val

liftMemoT :: MonadA m => StateT Cache (T m) a -> ExprGuiM m a
liftMemoT = liftMemo . mapStateT lift

memo ::
  (Cache.Key k, Binary v, Typeable v, MonadA m) =>
  Cache.FuncId ->
  (k -> WidgetEnvT (T m) v) -> k -> ExprGuiM m v
memo funcId f key = liftMemo $ Cache.memoS funcId f key

memoT ::
  (Cache.Key k, Binary v, Typeable v, MonadA m) =>
  Cache.FuncId -> (k -> T m v) -> k -> ExprGuiM m v
memoT funcId f = memo funcId (lift . f)

run ::
  MonadA m =>
  (ParentPrecedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
  Anchors.CodeProps m -> Settings -> ExprGuiM m a ->
  StateT Cache (WidgetEnvT (T m)) a
run makeSubexpr codeAnchors settings (ExprGuiM action) =
  StateT $ \cache ->
  fmap f $ runRWST action
  Askable
  { _aSettings = settings
  , _aMakeSubexpression = makeSubexpr
  , _aCodeAnchors = codeAnchors
  }
  (GuiState cache)
  where
    f (x, GuiState newCache, _output) = (x, newCache)

widgetEnv :: MonadA m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv = ExprGuiM . lift

transaction :: MonadA m => T m a -> ExprGuiM m a
transaction = widgetEnv . lift

getP :: MonadA m => Transaction.MkProperty m a -> ExprGuiM m a
getP = transaction . Transaction.getP

getCodeAnchor ::
  MonadA m => (Anchors.CodeProps m -> Transaction.MkProperty m b) -> ExprGuiM m b
getCodeAnchor anchor = getP . anchor =<< readCodeAnchors

assignCursor :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursor x y = localEnv $ WE.envAssignCursor x y

assignCursorPrefix :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursorPrefix x y = localEnv $ WE.envAssignCursorPrefix x y

wrapDelegated ::
  (MonadA f, MonadA m) =>
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  ((Widget f -> Widget f) -> a -> b) ->
  (Widget.Id -> ExprGuiM m a) ->
  Widget.Id -> ExprGuiM m b
wrapDelegated =
  BWidgets.wrapDelegatedWith (widgetEnv WE.readCursor) (widgetEnv WE.readConfig)
  (localEnv . (WE.envCursor %~))

-- Used vars:

listener :: MonadA m => (Output m -> b) -> ExprGuiM m a -> ExprGuiM m (a, b)
listener f =
  exprGuiM %~ RWS.listen
  & Lens.mapped . Lens.mapped . Lens._2 %~ f

listenUsedVariables :: MonadA m => ExprGuiM m a -> ExprGuiM m (a, [Guid])
listenUsedVariables = listener oAccessedVars

listenResultPickers :: MonadA m => ExprGuiM m a -> ExprGuiM m (a, [T m ()])
listenResultPickers = listener oHolePickers

markVariablesAsUsed :: MonadA m => AccessedVars -> ExprGuiM m ()
markVariablesAsUsed vars = ExprGuiM $ RWS.tell mempty { oAccessedVars = vars }

addResultPicker :: MonadA m => T m () -> ExprGuiM m ()
addResultPicker picker = ExprGuiM $ RWS.tell mempty { oHolePickers = [picker] }
