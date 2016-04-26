{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Monad
    ( ExprGuiM
    , widgetEnv
    , makeLabel
    , StoredEntityIds(..), Injected(..)
    , transaction, localEnv, withFgColor, withLocalUnderline
    , getP, assignCursor, assignCursorPrefix
    , makeFocusDelegator
    --
    , makeSubexpression
    , advanceDepth
    --
    , readConfig, readSettings, readStyle, readCodeAnchors
    , getCodeAnchor, mkPrejumpPosSaver
    , vspacer
    --
    , readMScopeId, withLocalMScopeId
    , isExprSelected
    --
    , outerPrecedence
    , withLocalPrecedence
    --
    , HolePickers, holePickersAddDocPrefix, holePickersAction
    , addResultPicker, listenResultPickers
    , run
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS (RWST, runRWST)
import qualified Control.Monad.Trans.RWS as RWS
import           Data.Binary (Binary)
import qualified Data.Char as Char
import           Data.CurAndPrev (CurAndPrev)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation.Id (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.WidgetId (toAnimId)
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Results (ScopeId, topLevelScopeId)
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.Precedence (Precedence)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (Style)
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

type HolePickers m = [T m Widget.EventResult]

holePickersAddDocPrefix :: HolePickers m -> E.Subtitle -> E.Subtitle
holePickersAddDocPrefix [] doc = doc
holePickersAddDocPrefix (_:_) doc =
    doc
    & Lens.element 0 %~ Char.toLower
    & ("Pick result and " ++)

holePickersAction :: Monad m => HolePickers m -> T m Widget.EventResult
holePickersAction = fmap mconcat . sequence

newtype Output m = Output
    { oHolePickers :: HolePickers m
    } deriving (Monoid)

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving (Monoid)

newtype Injected = Injected [Bool]
    deriving (Monoid, Binary, Eq, Ord)

data Askable m = Askable
    { _aSettings :: Settings
    , _aConfig :: Config
    , _aMakeSubexpression :: ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
    , _aCodeAnchors :: Anchors.CodeProps m
    , _aSubexpressionLayer :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aOuterPrecedence :: Precedence
    , _aStyle :: Style
    }

newtype ExprGuiM m a = ExprGuiM
    { _exprGuiM :: RWST (Askable m) (Output m) () (WidgetEnvT (T m)) a
    }
    deriving (Functor, Applicative, Monad)

Lens.makeLenses ''Askable
Lens.makeLenses ''ExprGuiM

-- TODO: To lens
localEnv :: Monad m => (WE.Env -> WE.Env) -> ExprGuiM m a -> ExprGuiM m a
localEnv = (exprGuiM %~) . RWS.mapRWST . WE.localEnv

withFgColor :: Monad m => Draw.Color -> ExprGuiM m a -> ExprGuiM m a
withFgColor = localEnv . WE.setTextColor

withLocalUnderline :: Monad m => TextView.Underline -> ExprGuiM m a -> ExprGuiM m a
withLocalUnderline underline =
    WE.envTextStyle . TextEdit.sTextViewStyle .
    TextView.styleUnderline ?~ underline
    & localEnv

readStyle :: Monad m => ExprGuiM m Style
readStyle = ExprGuiM $ Lens.view aStyle

readSettings :: Monad m => ExprGuiM m Settings
readSettings = ExprGuiM $ Lens.view aSettings

readConfig :: Monad m => ExprGuiM m Config
readConfig = ExprGuiM $ Lens.view aConfig

readCodeAnchors :: Monad m => ExprGuiM m (Anchors.CodeProps m)
readCodeAnchors = ExprGuiM $ Lens.view aCodeAnchors

mkPrejumpPosSaver :: Monad m => ExprGuiM m (T m ())
mkPrejumpPosSaver =
    DataOps.savePreJumpPosition <$> readCodeAnchors <*> widgetEnv WE.readCursor

-- | Vertical spacer as ratio of line height
vspacer :: (Real a, Monad m) => (Config -> a) -> ExprGuiM m (Widget f)
vspacer configGetter =
    (*)
    <$> widgetEnv BWidgets.stdFontHeight
    <*> ( readConfig
          <&> configGetter
          <&> realToFrac )
    <&> BWidgets.vspaceWidget

makeSubexpression ::
    Monad m =>
    (Precedence -> Precedence) -> ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpression onPrecedence expr =
    advanceDepth (return . Layout.fromCenteredWidget . Widget.fromView) animId $
    do
        maker <- ExprGuiM $ Lens.view aMakeSubexpression
        maker expr & withLocalPrecedence onPrecedence
    where
        animId = toAnimId $ WidgetIds.fromExprPayload $ expr ^. Sugar.rPayload

advanceDepth ::
    Monad m => (View -> ExprGuiM m r) ->
    AnimId -> ExprGuiM m r -> ExprGuiM m r
advanceDepth f animId action =
    do
        maxDepth <- readConfig <&> Config.maxExprDepth
        depth <- ExprGuiM $ Lens.view aSubexpressionLayer
        if depth >= maxDepth
            then mkErrorWidget >>= f
            else action & exprGuiM %~ RWS.local (aSubexpressionLayer +~ 1)
    where
        mkErrorWidget =
            BWidgets.makeTextView "ERROR: Subexpr too deep" animId
            & widgetEnv

run ::
    Monad m =>
    (ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
    Anchors.CodeProps m -> Config -> Settings -> Style -> ExprGuiM m a ->
    WidgetEnvT (T m) a
run makeSubexpr codeAnchors config settings style (ExprGuiM action) =
    f <$> runRWST action
    Askable
    { _aConfig = config
    , _aSettings = settings
    , _aMakeSubexpression = makeSubexpr
    , _aCodeAnchors = codeAnchors
    , _aSubexpressionLayer = 0
    , _aMScopeId = Just topLevelScopeId & pure
    , _aOuterPrecedence = 0
    , _aStyle = style
    }
    ()
    where
        f (x, (), _output) = x

widgetEnv :: Monad m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv = ExprGuiM . lift

makeLabel ::
    Monad m => String -> AnimId -> ExprGuiM m (Widget f)
makeLabel text animId = widgetEnv $ BWidgets.makeLabel text animId

transaction :: Monad m => T m a -> ExprGuiM m a
transaction = widgetEnv . lift

getP :: Monad m => Transaction.MkProperty m a -> ExprGuiM m a
getP = transaction . Transaction.getP

getCodeAnchor ::
    Monad m => (Anchors.CodeProps m -> Transaction.MkProperty m b) -> ExprGuiM m b
getCodeAnchor anchor = getP . anchor =<< readCodeAnchors

assignCursor ::
    Monad m => Widget.Id -> Widget.Id ->
    ExprGuiM m a -> ExprGuiM m a
assignCursor x y = localEnv $ WE.envAssignCursor x y

assignCursorPrefix ::
    Monad m => Widget.Id -> (AnimId -> Widget.Id) ->
    ExprGuiM m a -> ExprGuiM m a
assignCursorPrefix x y = localEnv $ WE.envAssignCursorPrefix x y

makeFocusDelegator ::
    (Applicative f, Monad m) =>
    FocusDelegator.Config ->
    FocusDelegator.FocusEntryTarget ->
    Widget.Id ->
    Widget f -> ExprGuiM m (Widget f)
makeFocusDelegator =
    BWidgets.makeFocusDelegator
    & Lens.mapped . Lens.mapped . Lens.mapped . Lens.mapped %~ widgetEnv

-- Used vars:

listener :: Monad m => (Output m -> b) -> ExprGuiM m a -> ExprGuiM m (a, b)
listener f =
    exprGuiM %~ RWS.listen
    & Lens.mapped . Lens.mapped . _2 %~ f

listenResultPickers :: Monad m => ExprGuiM m a -> ExprGuiM m (a, HolePickers m)
listenResultPickers = listener oHolePickers

addResultPicker :: Monad m => T m Widget.EventResult -> ExprGuiM m ()
addResultPicker picker = ExprGuiM $ RWS.tell mempty { oHolePickers = [picker] }

readMScopeId :: Monad m => ExprGuiM m (CurAndPrev (Maybe ScopeId))
readMScopeId = ExprGuiM $ Lens.view aMScopeId

withLocalMScopeId ::
    Monad m => CurAndPrev (Maybe ScopeId) -> ExprGuiM m a -> ExprGuiM m a
withLocalMScopeId mScopeId = exprGuiM %~ RWS.local (aMScopeId .~ mScopeId)

isExprSelected :: Monad m => Sugar.Payload f a -> ExprGuiM m Bool
isExprSelected = widgetEnv . WE.isSubCursor . WidgetIds.fromExprPayload

outerPrecedence :: Monad m => ExprGuiM m Precedence
outerPrecedence = ExprGuiM $ Lens.view aOuterPrecedence

withLocalPrecedence :: Monad m => (Precedence -> Precedence) -> ExprGuiM m a -> ExprGuiM m a
withLocalPrecedence f = exprGuiM %~ RWS.local (aOuterPrecedence %~ f)
