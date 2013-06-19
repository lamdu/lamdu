{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies, DeriveDataTypeable #-}
module Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad
  ( ExprGuiM, WidgetT, runWidget
  , widgetEnv
  , Payload(..), SugarExpr

  , transaction, localEnv, withFgColor
  , getP, assignCursor, assignCursorPrefix
  , wrapDelegated
  --
  , makeSubexpresion
  --
  , readSettings, readCodeAnchors
  , getCodeAnchor, mkPrejumpPosSaver
  --
  , addResultPicker, listenResultPickers

  , AccessedVars, markVariablesAsUsed, listenUsedVariables

  , memo, memoT
  , liftMemo, liftMemoT

  , HoleNumber, nextHoleNumber
  , inCollapsedExpression
  , isInCollapsedExpression

  , appendToTopLevelEventMap
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Cache (Cache)
import Data.Derive.Binary (makeBinary)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Types (ExpressionGui, WidgetT, ParentPrecedence(..), Precedence)
import Lamdu.CodeEdit.Settings (Settings)
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetEnvT as WE

type T = Transaction
type AccessedVars = [Guid]

data Output m = Output
  { oAccessedVars :: AccessedVars
  , oHolePickers :: [Sugar.PrefixAction m]
  , oTopLevelEventMap :: Widget.EventHandlers (T m)
  }
derive makeMonoid ''Output

data Payload = Payload
  { plGuids :: [Guid]
  , plInjected :: [Bool]
  } deriving (Typeable)
derive makeMonoid ''Payload
derive makeBinary ''Payload

type SugarExpr m = Sugar.ExpressionN m Payload

data Askable m = Askable
  { _aSettings :: Settings
  , _aMakeSubexpression ::
    ParentPrecedence ->
    Sugar.ExpressionN m Payload ->
    ExprGuiM m (ExpressionGui m)
  , _aCodeAnchors :: Anchors.CodeProps m
  , _aInCollapsedExpression :: Bool
  }

type HoleNumber = Int

data GuiState = GuiState
  { _gsCache :: Cache
  , _gsNextHoleNumber :: HoleNumber
  }

newtype ExprGuiM m a = ExprGuiM
  { _exprGuiM :: RWST (Askable m) (Output m) GuiState (WidgetEnvT (T m)) a
  }
  deriving (Functor, Applicative, Monad)

Lens.makeLenses ''Askable
Lens.makeLenses ''ExprGuiM
Lens.makeLenses ''GuiState

nextHoleNumber :: MonadA m => ExprGuiM m HoleNumber
nextHoleNumber = ExprGuiM $
  Lens.use gsNextHoleNumber <* (gsNextHoleNumber += 1)

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
makeSubexpresion ::
  MonadA m => Precedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpresion parentPrecedence expr = do
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
  (Cache.Key k, Binary v, MonadA m) =>
  (k -> WidgetEnvT (T m) v) -> k -> ExprGuiM m v
memo f key = liftMemo $ Cache.memoS f key

memoT ::
  (Cache.Key k, Binary v, MonadA m) =>
  (k -> T m v) -> k -> ExprGuiM m v
memoT f = memo (lift . f)

inCollapsedExpression :: MonadA m => ExprGuiM m a -> ExprGuiM m a
inCollapsedExpression =
  exprGuiM %~ RWS.local (aInCollapsedExpression .~ True)

isInCollapsedExpression :: MonadA m => ExprGuiM m Bool
isInCollapsedExpression = ExprGuiM $ Lens.view aInCollapsedExpression

run ::
  MonadA m =>
  (ParentPrecedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
  Anchors.CodeProps m -> Settings -> ExprGuiM m a ->
  StateT Cache (WidgetEnvT (T m)) (Widget.EventHandlers (T m), a)
run makeSubexpression codeAnchors settings (ExprGuiM action) =
  StateT $ \cache ->
  fmap f $ runRWST action
  Askable
  { _aSettings = settings
  , _aMakeSubexpression = makeSubexpression
  , _aCodeAnchors = codeAnchors
  , _aInCollapsedExpression = False
  }
  (GuiState cache 1)
  where
    f (x, GuiState newCache _, output) = ((oTopLevelEventMap output, x), newCache)

runWidget ::
  MonadA m =>
  (ParentPrecedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
  Anchors.CodeProps m -> Settings -> ExprGuiM m (Widget (T m)) ->
  StateT Cache (WidgetEnvT (T m)) (Widget (T m))
runWidget makeSubexpression codeAnchors settings action =
  uncurry Widget.weakerEvents <$> run makeSubexpression codeAnchors settings action

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

appendToTopLevelEventMap :: MonadA m => Widget.EventHandlers (T m) -> ExprGuiM m ()
appendToTopLevelEventMap eventMap =
  ExprGuiM $ RWS.tell mempty { oTopLevelEventMap = eventMap }
