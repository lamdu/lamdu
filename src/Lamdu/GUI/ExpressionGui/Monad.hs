{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Lamdu.GUI.ExpressionGui.Monad
    ( StoredEntityIds(..)
    --
    , advanceDepth, resetDepth
    --
    , mkPrejumpPosSaver
    --
    , readMScopeId, withLocalMScopeId
    --
    , MonadExprGui(..)
    , ExprGuiM, run
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Property as Property
import           Data.Vector.Vector2 (Vector2)
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.State (GUIState(..))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget.Id (toAnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme, HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (ScopeId, topLevelScopeId)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Settings (Settings, HasSettings(..))
import           Lamdu.Style (Style, HasStyle(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving (Semigroup, Monoid)

data Askable m = Askable
    { _aState :: GUIState
    , _aTextEditStyle :: TextEdit.Style
    , _aStdSpacing :: Vector2 Double
    , _aAnimIdPrefix :: AnimId
    , _aSettings :: Settings
    , _aConfig :: Config
    , _aTheme :: Theme
    , _aMakeSubexpression :: ExprGui.SugarExpr (T (TM m)) -> m (ExpressionGui (T (TM m)))
    , _aGuiAnchors :: Anchors.GuiAnchors (TM m)
    , _aDepthLeft :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aStyle :: Style
    }

class
    ( MonadTransaction (TM m) m, MonadReader (Askable m) m
    ) => MonadExprGui m where
    type TM m :: * -> *
    makeSubexpression :: ExprGui.SugarExpr (T (TM m)) -> m (ExpressionGui (T (TM m)))

Lens.makeLenses ''Askable

instance GuiState.HasCursor (Askable m)
instance GuiState.HasState (Askable m) where state = aState
instance TextView.HasStyle (Askable m) where style = aTextEditStyle . TextView.style
instance TextEdit.HasStyle (Askable m) where style = aTextEditStyle
instance Spacer.HasStdSpacing (Askable m) where stdSpacing = aStdSpacing
instance Element.HasAnimIdPrefix (Askable m) where animIdPrefix = aAnimIdPrefix
instance Config.HasConfig (Askable m) where config = aConfig
instance HasTheme (Askable m) where theme = aTheme
instance ResponsiveExpr.HasStyle (Askable m) where style = aTheme . ResponsiveExpr.style
instance Menu.HasConfig (Askable m) where
    config f askable =
        f Menu.Config
        { Menu._configKeys = askable ^. aConfig . Config.menu
        , Menu._configStyle = askable ^. aTheme . Theme.menu
        }
        <&>
        \menuConfig ->
        askable
        & aTheme . Theme.menu .~ menuConfig ^. Menu.configStyle
        & aConfig . Config.menu .~ menuConfig ^. Menu.configKeys
instance Hover.HasStyle (Askable m) where style = aTheme . Hover.style
instance HasStyle (Askable m) where style = aStyle
instance HasSettings (Askable m) where settings = aSettings

readGuiAnchors :: MonadExprGui m => m (Anchors.GuiAnchors (TM m))
readGuiAnchors = Lens.view aGuiAnchors

savePreJumpPosition :: Monad m => Anchors.GuiAnchors m -> WidgetId.Id -> T m ()
savePreJumpPosition guiAnchors pos = Property.modP (Anchors.preJumps guiAnchors) $ (pos :) . take 19

mkPrejumpPosSaver :: MonadExprGui m => m (T (TM m) ())
mkPrejumpPosSaver =
    savePreJumpPosition <$> readGuiAnchors <*> Lens.view GuiState.cursor

resetDepth :: MonadExprGui m => Int -> m r -> m r
resetDepth depth = Reader.local (aDepthLeft .~ depth)

advanceDepth :: MonadExprGui m => (WithTextPos View -> m r) -> AnimId -> m r -> m r
advanceDepth f animId action =
    do
        depth <- Lens.view aDepthLeft
        if depth <= 0
            then mkErrorWidget >>= f
            else action & Reader.local (aDepthLeft -~ 1)
    where
        mkErrorWidget = TextView.make ?? "..." ?? animId

readMScopeId :: MonadExprGui m => m (CurAndPrev (Maybe ScopeId))
readMScopeId = Lens.view aMScopeId

withLocalMScopeId :: MonadExprGui m => CurAndPrev (Maybe ScopeId) -> m a -> m a
withLocalMScopeId mScopeId = Reader.local (aMScopeId .~ mScopeId)

newtype ExprGuiM m a = ExprGuiM (ReaderT (Askable (ExprGuiM m)) (T m) a)
    deriving (Functor, Applicative, Monad, MonadReader (Askable (ExprGuiM m)))

instance (Monad m, Semigroup a) => Semigroup (ExprGuiM m a) where
    (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (ExprGuiM m a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Monad m => MonadTransaction m (ExprGuiM m) where
    transaction = ExprGuiM . lift

instance Monad m => MonadExprGui (ExprGuiM m) where
    type TM (ExprGuiM m) = m
    makeSubexpression expr =
        do
            maker <- Lens.view aMakeSubexpression
            maker expr
        & advanceDepth (pure . Responsive.fromTextView) animId
        where
            animId = expr ^. Sugar.rPayload & WidgetIds.fromExprPayload & toAnimId

run ::
    ( MonadTransaction m n, MonadReader env n
    , GuiState.HasState env, Spacer.HasStdSpacing env
    , Config.HasConfig env, HasTheme env
    , HasSettings env, HasStyle env
    ) =>
    (ExprGui.SugarExpr (T m) -> ExprGuiM m (ExpressionGui (T m))) ->
    Anchors.GuiAnchors m ->
    ExprGuiM m a ->
    n a
run makeSubexpr theGuiAnchors (ExprGuiM action) =
    do
        theSettings <- Lens.view settings
        theStyle <- Lens.view style
        theState <- Lens.view GuiState.state
        theTextEditStyle <- Lens.view TextEdit.style
        theStdSpacing <- Lens.view Spacer.stdSpacing
        theConfig <- Lens.view Config.config
        theTheme <- Lens.view Theme.theme
        runReaderT action
            Askable
            { _aState = theState
            , _aTextEditStyle = theTextEditStyle
            , _aStdSpacing = theStdSpacing
            , _aAnimIdPrefix = ["outermost"]
            , _aConfig = theConfig
            , _aTheme = theTheme
            , _aSettings = theSettings
            , _aMakeSubexpression = makeSubexpr
            , _aGuiAnchors = theGuiAnchors
            , _aDepthLeft = theConfig ^. Config.maxExprDepth
            , _aMScopeId = Just topLevelScopeId & pure
            , _aStyle = theStyle
            } & transaction
