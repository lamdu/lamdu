{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Lamdu.GUI.ExpressionGui.Monad
    ( ExprGuiM, Askable
    , widgetEnv
    , makeLabel
    , StoredEntityIds(..)
    , withLocalUnderline
    --
    , makeSubexpression
    , makeSubexpressionWith
    , advanceDepth, resetDepth
    --
    , readConfig, readTheme, readMinOpPrec, readSettings, readStyle
    , readCodeAnchors, mkPrejumpPosSaver
    , vspacer
    --
    , readMScopeId, withLocalMScopeId
    , isExprSelected
    --
    , outerPrecedence
    , withLocalPrecedence
    --
    , readVerbose, withVerbose
    --
    , HolePicker(..), withHolePicker
    , setResultPicker, listenResultPicker
    , run
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Trans.FastRWS (RWST, runRWST)
import qualified Control.Monad.Trans.FastRWS as RWS
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Char as Char
import           Data.CurAndPrev (CurAndPrev)
import           Data.Store.Transaction (Transaction)
import qualified Data.Text.Lens as TextLens
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation.Id (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Id (toAnimId)
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Results (ScopeId, topLevelScopeId)
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.Precedence (Precedence)
import qualified Lamdu.GUI.Precedence as Precedence
import qualified Lamdu.GUI.Spacing as Spacing
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.WidgetsEnvT (WidgetEnvT)
import qualified Lamdu.GUI.WidgetsEnvT as WE
import           Lamdu.Style (Style)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data HolePicker m
    = NoHolePick
    | HolePick (T m Widget.EventResult)

instance Monoid (HolePicker m) where
    mempty = NoHolePick
    mappend NoHolePick x = x
    mappend x NoHolePick = x
    mappend _ _ = error "Two HolePick's told, are we inside 2 holes simultaneously?"

withHolePicker :: Monad m => HolePicker m -> E.EventMap (T m a) -> E.EventMap (T m a)
withHolePicker NoHolePick e = e
withHolePicker (HolePick action) e =
    e
    & E.emDocs . E.docStrs . Lens.reversed . Lens.element 0 %~ f
    <&> (action >>)
    where
        f x =
            x
            & TextLens._Text . Lens.element 0 %~ Char.toLower
            & ("Pick result and " <>)

newtype Output m = Output
    { oHolePicker :: HolePicker m
    } deriving (Monoid)

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving (Monoid)

data Askable m = Askable
    { _aWidgetEnv :: WE.Env
    , _aSettings :: Settings
    , _aConfig :: Config
    , _aTheme :: Theme
    , _aMakeSubexpression :: ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
    , _aCodeAnchors :: Anchors.CodeProps m
    , _aSubexpressionLayer :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aOuterPrecedence :: Precedence
    , _aMinOpPrecedence :: Int
      -- ^ The minimum precedence that operators must have to be
      -- applicable inside leaf holes. This allows "text-like"
      -- appending of operators at the right parent level according to
      --
      -- precedence (e.g: in "x % 3 == 0 || ..." the hole for "3" does
      -- not allow operators, making the == apply at the parent expr)
    , _aStyle :: Style
    , _aVerbose :: Bool
    }
newtype ExprGuiM m a = ExprGuiM
    { _exprGuiM :: RWST (Askable m) (Output m) () (T m) a
    } deriving (Functor, Applicative, Monad, MonadReader (Askable m))

Lens.makeLenses ''Askable
Lens.makeLenses ''ExprGuiM

instance Monad m => MonadTransaction m (ExprGuiM m) where transaction = ExprGuiM . lift

instance Widget.HasCursor (Askable m) where cursor = aWidgetEnv . Widget.cursor
instance TextView.HasStyle (Askable m) where style = aWidgetEnv . TextView.style
instance TextEdit.HasStyle (Askable m) where style = aWidgetEnv . TextEdit.style
instance Spacing.HasStdSpacing (Askable m) where
    stdSpacing = aWidgetEnv . Spacing.stdSpacing

withLocalUnderline :: Monad m => TextView.Underline -> ExprGuiM m a -> ExprGuiM m a
withLocalUnderline underline = Reader.local (TextView.underline ?~ underline)

readStyle :: Monad m => ExprGuiM m Style
readStyle = ExprGuiM $ Lens.view aStyle

readSettings :: Monad m => ExprGuiM m Settings
readSettings = ExprGuiM $ Lens.view aSettings

readConfig :: Monad m => ExprGuiM m Config
readConfig = ExprGuiM $ Lens.view aConfig

readTheme :: Monad m => ExprGuiM m Theme
readTheme = ExprGuiM $ Lens.view aTheme

readMinOpPrec :: Monad m => ExprGuiM m Int
readMinOpPrec = ExprGuiM $ Lens.view aMinOpPrecedence

readCodeAnchors :: Monad m => ExprGuiM m (Anchors.CodeProps m)
readCodeAnchors = ExprGuiM $ Lens.view aCodeAnchors

readVerbose :: Monad m => ExprGuiM m Bool
readVerbose = ExprGuiM $ Lens.view aVerbose

withVerbose :: ExprGuiM m a -> ExprGuiM m a
withVerbose = exprGuiM %~ RWS.local (aVerbose .~ True)

mkPrejumpPosSaver :: Monad m => ExprGuiM m (T m ())
mkPrejumpPosSaver =
    DataOps.savePreJumpPosition <$> readCodeAnchors <*> Lens.view Widget.cursor

-- | Vertical spacer as ratio of line height
vspacer :: Monad m => (Theme -> Double) -> ExprGuiM m (Widget f)
vspacer themeGetter = readTheme <&> themeGetter >>= Spacing.vspacer

makeSubexpression ::
    Monad m =>
    ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpression = makeSubexpressionWith 0 (const (Precedence.make 0))

makeSubexpressionWith ::
    Monad m =>
    Int -> (Precedence -> Precedence) -> ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeSubexpressionWith minOpPrec onPrecedence expr =
    do
        maker <- Lens.view aMakeSubexpression & ExprGuiM
        maker expr & withLocalPrecedence minOpPrec onPrecedence
    & advanceDepth (return . TreeLayout.fromCenteredView) animId
    where
        animId = toAnimId $ WidgetIds.fromExprPayload $ expr ^. Sugar.rPayload

resetDepth :: Int -> ExprGuiM m r -> ExprGuiM m r
resetDepth depth = exprGuiM %~ RWS.local (aSubexpressionLayer .~ depth)

advanceDepth ::
    Monad m => (View -> ExprGuiM m r) ->
    AnimId -> ExprGuiM m r -> ExprGuiM m r
advanceDepth f animId action =
    do
        depth <- ExprGuiM $ Lens.view aSubexpressionLayer
        if depth <= 0
            then mkErrorWidget >>= f
            else action & exprGuiM %~ RWS.local (aSubexpressionLayer -~ 1)
    where
        mkErrorWidget = TextView.make ?? "..." ?? animId

run ::
    Monad m =>
    (ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)) ->
    Anchors.CodeProps m -> Config -> Theme -> Settings -> Style ->
    ExprGuiM m a ->
    WidgetEnvT (T m) a
run makeSubexpr codeAnchors config theme settings style (ExprGuiM action) =
    do
        env <- WE.readEnv
        runRWST action
            Askable
            { _aWidgetEnv = env
            , _aConfig = config
            , _aTheme = theme
            , _aSettings = settings
            , _aMakeSubexpression = makeSubexpr
            , _aCodeAnchors = codeAnchors
            , _aSubexpressionLayer = Config.maxExprDepth config
            , _aMScopeId = Just topLevelScopeId & pure
            , _aOuterPrecedence = Precedence.make 0
            , _aMinOpPrecedence = 0
            , _aStyle = style
            , _aVerbose = False
            }
            ()
            <&> (\(x, (), _output) -> x)
            & lift

makeLabel :: Monad m => Text -> AnimId -> ExprGuiM m View
makeLabel text animId = TextView.makeLabel ?? text ?? animId

widgetEnv :: Monad m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv action = do
    env <- ExprGuiM $ Lens.view aWidgetEnv
    WE.runWidgetEnvT env action & transaction

-- Used vars:

listener :: Monad m => (Output m -> b) -> ExprGuiM m a -> ExprGuiM m (a, b)
listener f =
    exprGuiM %~ RWS.listen
    & Lens.mapped . Lens.mapped . _2 %~ f

listenResultPicker :: Monad m => ExprGuiM m a -> ExprGuiM m (a, HolePicker m)
listenResultPicker = listener oHolePicker

setResultPicker :: Monad m => T m Widget.EventResult -> ExprGuiM m ()
setResultPicker picker =
    ExprGuiM $ RWS.tell mempty { oHolePicker = HolePick picker }

readMScopeId :: Monad m => ExprGuiM m (CurAndPrev (Maybe ScopeId))
readMScopeId = ExprGuiM $ Lens.view aMScopeId

withLocalMScopeId :: CurAndPrev (Maybe ScopeId) -> ExprGuiM m a -> ExprGuiM m a
withLocalMScopeId mScopeId = exprGuiM %~ RWS.local (aMScopeId .~ mScopeId)

isExprSelected :: Monad m => Sugar.Payload f a -> ExprGuiM m Bool
isExprSelected pl = Widget.isSubCursor ?? WidgetIds.fromExprPayload pl

outerPrecedence :: Monad m => ExprGuiM m Precedence
outerPrecedence = ExprGuiM $ Lens.view aOuterPrecedence

withLocalPrecedence :: Int -> (Precedence -> Precedence) -> ExprGuiM m a -> ExprGuiM m a
withLocalPrecedence minOpPrec f =
    exprGuiM %~
    RWS.local
    ( (aOuterPrecedence %~ f)
    . (aMinOpPrecedence .~ minOpPrec)
    )
