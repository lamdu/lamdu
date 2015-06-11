{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies #-}
module Lamdu.GUI.ExpressionGui.Monad
    ( ExprGuiM
    , widgetEnv
    , makeLabel
    , StoredEntityIds(..), Injected(..)
    , Payload(..), plStoredEntityIds, plInjected, plNearestHoles, plShowAnnotation
    , ShowAnnotation(..)
    , markRedundantTypes
    , getInfoMode
    , emptyPayload
    , SugarExpr

    , transaction, localEnv, withFgColor
    , getP, assignCursor, assignCursorPrefix
    , makeFocusDelegator
    --
    , makeSubexpression
    --
    , readConfig, readSettings, readCodeAnchors
    , getCodeAnchor, mkPrejumpPosSaver
    --
    , HolePickers, holePickersAddDocPrefix, holePickersAction
    , addResultPicker, listenResultPickers
    , nextHolesBefore
    , run
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS (RWST, runRWST)
import qualified Control.Monad.Trans.RWS as RWS
import           Control.MonadA (MonadA)
import           Data.Binary (Binary)
import qualified Data.Char as Char
import           Data.Monoid (Monoid(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation.Id (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.WidgetId (toAnimId)
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import           Lamdu.Config (Config)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import           Lamdu.GUI.Precedence (ParentPrecedence(..), Precedence)
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (ExpressionN)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import           Lamdu.Sugar.RedundantTypes (redundantTypes)
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

data ShowAnnotation = ShowAnnotationInVerboseMode | DoNotShowAnnotation | ShowAnnotation

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plStoredEntityIds :: [Sugar.EntityId]
    , _plInjected :: [Bool]
    , _plNearestHoles :: NearestHoles
    , _plShowAnnotation :: ShowAnnotation
    }
Lens.makeLenses ''Payload

emptyPayload :: NearestHoles -> Payload
emptyPayload nearestHoles = Payload
    { _plStoredEntityIds = []
    , _plInjected = []
    , _plNearestHoles = nearestHoles
    , _plShowAnnotation = ShowAnnotationInVerboseMode
    }

type SugarExpr m = ExpressionN m Payload

data Askable m = Askable
    { _aSettings :: Settings
    , _aConfig :: Config
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

readConfig :: MonadA m => ExprGuiM m Config
readConfig = ExprGuiM $ Lens.view aConfig

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
        then
            mkErrorWidget <&> Layout.fromCenteredWidget & widgetEnv
        else do
            maker <- ExprGuiM $ Lens.view aMakeSubexpression
            maker (ParentPrecedence parentPrecedence) expr
                & exprGuiM %~ RWS.local (aSubexpressionLayer +~ 1)
    where
        widgetId = WidgetIds.fromExprPayload $ expr ^. Sugar.rPayload
        mkErrorWidget =
            BWidgets.makeTextViewWidget "ERROR: Subexpr too deep" (toAnimId widgetId)

run ::
    MonadA m =>
    (ParentPrecedence -> SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
    Anchors.CodeProps m -> Config -> Settings -> ExprGuiM m a ->
    WidgetEnvT (T m) a
run makeSubexpr codeAnchors config settings (ExprGuiM action) =
    f <$> runRWST action
    Askable
    { _aConfig = config
    , _aSettings = settings
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

assignCursor ::
    MonadA m => Widget.Id -> Widget.Id ->
    ExprGuiM m a -> ExprGuiM m a
assignCursor x y = localEnv $ WE.envAssignCursor x y

assignCursorPrefix ::
    MonadA m => Widget.Id -> (AnimId -> Widget.Id) ->
    ExprGuiM m a -> ExprGuiM m a
assignCursorPrefix x y = localEnv $ WE.envAssignCursorPrefix x y

makeFocusDelegator ::
    (Applicative f, MonadA m) =>
    FocusDelegator.Config ->
    FocusDelegator.FocusEntryTarget ->
    Widget.Id ->
    Widget f -> ExprGuiM m (Widget f)
makeFocusDelegator =
    BWidgets.makeFocusDelegator
    & Lens.mapped . Lens.mapped . Lens.mapped . Lens.mapped %~ widgetEnv

-- Used vars:

listener :: MonadA m => (Output m -> b) -> ExprGuiM m a -> ExprGuiM m (a, b)
listener f =
    exprGuiM %~ RWS.listen
    & Lens.mapped . Lens.mapped . _2 %~ f

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

getInfoMode :: MonadA m => ShowAnnotation -> ExprGuiM m CESettings.InfoMode
getInfoMode DoNotShowAnnotation = return CESettings.None
getInfoMode ShowAnnotationInVerboseMode = readSettings <&> (^. CESettings.sInfoMode)
getInfoMode ShowAnnotation =
    readSettings
    <&> (^. CESettings.sInfoMode)
    <&> \infoMode ->
            case infoMode of
            CESettings.None -> CESettings.Types
            x -> x

markRedundantTypes :: MonadA m => SugarExpr m -> SugarExpr m
markRedundantTypes v =
    v
    & redundantTypes         . showType .~ DoNotShowAnnotation
    & SugarLens.holePayloads . showType .~ ShowAnnotation
    & SugarLens.holeArgs     . showType .~ ShowAnnotation
    & Sugar.rPayload         . showType .~ ShowAnnotation
    where
        showType = Sugar.plData . plShowAnnotation
