{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts, UndecidableInstances, PolymorphicComponents #-}
module Lamdu.GUI.ExpressionGui.Monad
    ( StoredEntityIds(..)
    --
    , advanceDepth, resetDepth
    --
    , mkPrejumpPosSaver
    --
    , readMScopeId, withLocalMScopeId
    --
    , MonadExprGui(..), iam
    , ExprGuiM, ExprGuiM', run
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

import           Lamdu.Prelude

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
    , _aMakeSubexpression :: ExprGui.SugarExpr (IM m) (AM m) -> m (ExpressionGui (AM m))
    , _aGuiAnchors :: Anchors.GuiAnchors (IM m) (AM m)
    , _aDepthLeft :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aStyle :: Style
    , _aIam :: forall a. IM m a -> AM m a
    }

class (Monad (IM m), Monad (AM m), MonadReader (Askable m) m) => MonadExprGui m where
    type AM m :: * -> *         -- Sugar Action monad
    type IM m :: * -> *         -- Sugar Info Monad
    im :: IM m a -> m a
    makeSubexpression :: ExprGui.SugarExpr (IM m) (AM m) -> m (ExpressionGui (AM m))

Lens.makeLenses ''Askable

iam :: MonadExprGui m => m (IM m a -> AM m a)
iam = Lens.view aIam

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

readGuiAnchors :: MonadExprGui m => m (Anchors.GuiAnchors (IM m) (AM m))
readGuiAnchors = Lens.view aGuiAnchors

mkPrejumpPosSaver :: MonadExprGui m => m (AM m ())
mkPrejumpPosSaver =
    do
        preJumpsMkProp <- readGuiAnchors <&> Anchors.preJumps
        preJumpsProp <- preJumpsMkProp ^. Property.mkProperty & im
        cursor <- Lens.view GuiState.cursor
        Property.pureModify preJumpsProp ((cursor:) . take 19) & pure

resetDepth :: MonadExprGui m => Int -> m r -> m r
resetDepth depth = Reader.local (aDepthLeft .~ depth)

advanceDepth :: MonadExprGui m => (WithTextPos View -> m r) -> m r -> m r
advanceDepth f action =
    do
        depth <- Lens.view aDepthLeft
        if depth <= 0
            then mkErrorWidget >>= f
            else action & Reader.local (aDepthLeft -~ 1)
    where
        mkErrorWidget = TextView.makeLabel "..."

readMScopeId :: MonadExprGui m => m (CurAndPrev (Maybe ScopeId))
readMScopeId = Lens.view aMScopeId

withLocalMScopeId :: MonadExprGui m => CurAndPrev (Maybe ScopeId) -> m a -> m a
withLocalMScopeId mScopeId = Reader.local (aMScopeId .~ mScopeId)

newtype ExprGuiM im (am :: * -> *) a =
    ExprGuiM (ReaderT (Askable (ExprGuiM im am)) im a)
    deriving (Functor, Applicative, Monad, MonadReader (Askable (ExprGuiM im am)))

type ExprGuiM' m = ExprGuiM m m

instance (Monad im, Semigroup a) => Semigroup (ExprGuiM im am a) where
    (<>) = liftA2 (<>)

instance (Monad im, Monoid a) => Monoid (ExprGuiM im am a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance MonadTransaction n im => MonadTransaction n (ExprGuiM im am) where
    transaction = ExprGuiM . lift . transaction

instance (Monad im, Monad am) => MonadExprGui (ExprGuiM im am) where
    type IM (ExprGuiM im am) = im
    type AM (ExprGuiM im am) = am
    makeSubexpression expr =
        do
            maker <- Lens.view aMakeSubexpression
            maker expr
        & advanceDepth (pure . Responsive.fromTextView)
        & Reader.local (Element.animIdPrefix .~ animId)
        where
            animId = expr ^. Sugar.rPayload & WidgetIds.fromExprPayload & toAnimId
    im = ExprGuiM . lift

run ::
    ( GuiState.HasState env, Spacer.HasStdSpacing env
    , Config.HasConfig env, HasTheme env
    , HasSettings env, HasStyle env
    ) =>
    (ExprGui.SugarExpr im am -> ExprGuiM im am (ExpressionGui am)) ->
    Anchors.GuiAnchors im am ->
    env -> (forall x. im x -> am x) -> ExprGuiM im am a -> im a
run makeSubexpr theGuiAnchors env liftIam (ExprGuiM action) =
    runReaderT action
    Askable
    { _aState = env ^. GuiState.state
    , _aTextEditStyle = env ^. TextEdit.style
    , _aStdSpacing = env ^. Spacer.stdSpacing
    , _aAnimIdPrefix = ["outermost"]
    , _aConfig = env ^. Config.config
    , _aTheme = env ^. Theme.theme
    , _aSettings = env ^. settings
    , _aMakeSubexpression = makeSubexpr
    , _aGuiAnchors = theGuiAnchors
    , _aDepthLeft = env ^. Config.config . Config.maxExprDepth
    , _aMScopeId = Just topLevelScopeId & pure
    , _aStyle = env ^. style
    , _aIam = liftIam
    }
