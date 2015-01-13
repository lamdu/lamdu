{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies #-}
module Lamdu.GUI.ExpressionGui.Monad
  ( ExprGuiM, WidgetT
  , widgetEnv
  , makeLabel
  , StoredEntityIds(..), Injected(..)
  , Payload(..), plStoredEntityIds, plInjected, plNearestHoles, plShowType
  , ShowType(..)
  , markRedundantTypes
  , shouldShowType
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
  , HolePickers, holePickersAddDocPrefix, holePickersAction
  , addResultPicker, listenResultPickers
  , nextHolesBefore
  , run
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation.Id (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.WidgetId (toAnimId)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Types (ExpressionGui(..), WidgetT)
import Lamdu.GUI.Precedence (ParentPrecedence(..), Precedence)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import Lamdu.Sugar.AddNames.Types (ExpressionN)
import Lamdu.Sugar.NearestHoles (NearestHoles)
import Lamdu.Sugar.RedundantTypes (redundantTypes)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Char as Char
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

type HolePickers m = [T m Widget.EventResult]

holePickersAddDocPrefix :: HolePickers m -> E.Subtitle -> E.Subtitle
holePickersAddDocPrefix [] doc = doc
holePickersAddDocPrefix (_:_) doc =
  doc
  & Lens.element 0 %~ Char.toLower
  & ("Pick result and " ++)

holePickersAction :: MonadA m => HolePickers m -> T m Widget.EventResult
holePickersAction = fmap mconcat . sequence

newtype Output m = Output
  { oHolePickers :: HolePickers m
  } deriving (Monoid)

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
  deriving (Monoid)

newtype Injected = Injected [Bool]
  deriving (Monoid, Binary, Eq, Ord)

data ShowType = ShowTypeInVerboseMode | DoNotShowType | ShowType

-- GUI input payload on sugar exprs
data Payload = Payload
  { _plStoredEntityIds :: [Sugar.EntityId]
  , _plInjected :: [Bool]
  , _plNearestHoles :: NearestHoles
  , _plShowType :: ShowType
  }
Lens.makeLenses ''Payload

emptyPayload :: NearestHoles -> Payload
emptyPayload nearestHoles = Payload
  { _plStoredEntityIds = []
  , _plInjected = []
  , _plNearestHoles = nearestHoles
  , _plShowType = ShowTypeInVerboseMode
  }

type SugarExpr m = ExpressionN m Payload

data Askable m = Askable
  { _aSettings :: Settings
  , _aMakeSubexpression ::
    ParentPrecedence -> SugarExpr m ->
    ExprGuiM m (ExpressionGui m)
  , _aCodeAnchors :: Anchors.CodeProps m
  , _aSubexpressionLayer :: Int
  }

newtype ExprGuiM m a = ExprGuiM
  { _exprGuiM :: RWST (Askable m) (Output m) () (WidgetEnvT (T m)) a
  }
  deriving (Functor, Applicative, Monad)

Lens.makeLenses ''Askable
Lens.makeLenses ''ExprGuiM

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

makeSubexpression ::
  MonadA m => Precedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpression parentPrecedence expr = do
  depth <- ExprGuiM $ Lens.view aSubexpressionLayer
  if depth >= 15
    then widgetEnv $ (`ExpressionGui` 0.5) <$> mkErrorWidget
    else do
      maker <- ExprGuiM $ Lens.view aMakeSubexpression
      maker (ParentPrecedence parentPrecedence) expr
        & exprGuiM %~ RWS.local (aSubexpressionLayer +~ 1)
  where
    widgetId = WidgetIds.fromEntityId $ expr ^. Sugar.rPayload . Sugar.plEntityId
    mkErrorWidget =
      BWidgets.makeTextViewWidget "ERROR: Subexpr too deep" (toAnimId widgetId)

run ::
  MonadA m =>
  (ParentPrecedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
  Anchors.CodeProps m -> Settings -> ExprGuiM m a ->
  WidgetEnvT (T m) a
run makeSubexpr codeAnchors settings (ExprGuiM action) =
  f <$> runRWST action
  Askable
  { _aSettings = settings
  , _aMakeSubexpression = makeSubexpr
  , _aCodeAnchors = codeAnchors
  , _aSubexpressionLayer = 0
  }
  ()
  where
    f (x, (), _output) = x

widgetEnv :: MonadA m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv = ExprGuiM . lift

makeLabel ::
  MonadA m => String -> AnimId -> ExprGuiM m (Widget f)
makeLabel text animId = widgetEnv $ BWidgets.makeLabel text animId

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

listenResultPickers :: MonadA m => ExprGuiM m a -> ExprGuiM m (a, HolePickers m)
listenResultPickers = listener oHolePickers

addResultPicker :: MonadA m => T m Widget.EventResult -> ExprGuiM m ()
addResultPicker picker = ExprGuiM $ RWS.tell mempty { oHolePickers = [picker] }

leftMostLeaf :: Sugar.Expression name m a -> Sugar.Expression name m a
leftMostLeaf val =
  case val ^.. Sugar.rBody . Lens.traversed of
  [] -> val
  (x:_) -> leftMostLeaf x

nextHolesBefore :: Sugar.Expression name m Payload -> NearestHoles
nextHolesBefore val =
  node ^. Sugar.rPayload . Sugar.plData . plNearestHoles
  & if Lens.has (Sugar.rBody . Sugar._BodyHole) node
    then NearestHoles.next .~ Just (node ^. Sugar.rPayload . Sugar.plEntityId)
    else id
  where
    node = leftMostLeaf val

shouldShowType :: MonadA m => ShowType -> ExprGuiM m Bool
shouldShowType DoNotShowType = return False
shouldShowType ShowType = return True
shouldShowType ShowTypeInVerboseMode = do
  infoMode <- (^. CESettings.sInfoMode) <$> readSettings
  return $
    case infoMode of
    CESettings.None -> False
    CESettings.Types -> True

markRedundantTypes :: MonadA m => SugarExpr m -> SugarExpr m
markRedundantTypes v =
  v
  & redundantTypes         . showType .~ DoNotShowType
  & SugarLens.holePayloads . showType .~ ShowType
  & SugarLens.holeArgs     . showType .~ ShowType
  & Sugar.rPayload         . showType .~ ShowType
  where
    showType = Sugar.plData . plShowType
