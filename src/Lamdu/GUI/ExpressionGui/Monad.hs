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
    , im, iam, makeSubexpression
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
import           Lamdu.Name (Name)
import           Lamdu.Settings (Settings, HasSettings(..))
import           Lamdu.Style (Style, HasStyle(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving (Semigroup, Monoid)

data Askable im am = Askable
    { _aState :: GUIState
    , _aTextEditStyle :: TextEdit.Style
    , _aStdSpacing :: Vector2 Double
    , _aAnimIdPrefix :: AnimId
    , _aSettings :: Settings
    , _aConfig :: Config
    , _aTheme :: Theme
    , _aMakeSubexpression :: ExprGui.SugarExpr im am -> ExprGuiM im am (ExpressionGui am)
    , _aGuiAnchors :: Anchors.GuiAnchors im am
    , _aDepthLeft :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aStyle :: Style
    , _aIam :: forall x. im x -> am x
    }

newtype ExprGuiM im (am :: * -> *) a =
    ExprGuiM (ReaderT (Askable im am) im a)
    deriving (Functor, Applicative, Monad, MonadReader (Askable im am))

-- TODO: Remove this:
type ExprGuiM' m = ExprGuiM m m

Lens.makeLenses ''Askable

instance GuiState.HasCursor (Askable im am)
instance GuiState.HasState (Askable im am) where state = aState
instance TextView.HasStyle (Askable im am) where style = aTextEditStyle . TextView.style
instance TextEdit.HasStyle (Askable im am) where style = aTextEditStyle
instance Spacer.HasStdSpacing (Askable im am) where stdSpacing = aStdSpacing
instance Element.HasAnimIdPrefix (Askable im am) where animIdPrefix = aAnimIdPrefix
instance Config.HasConfig (Askable im am) where config = aConfig
instance HasTheme (Askable im am) where theme = aTheme
instance ResponsiveExpr.HasStyle (Askable im am) where style = aTheme . ResponsiveExpr.style
instance Menu.HasConfig (Askable im am) where
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
instance Hover.HasStyle (Askable im am) where style = aTheme . Hover.style
instance HasStyle (Askable im am) where style = aStyle
instance HasSettings (Askable im am) where settings = aSettings

im :: Monad im => im a -> ExprGuiM im am a
im = ExprGuiM . lift

iam :: Monad im => ExprGuiM im am (im a -> am a)
iam = Lens.view aIam

readGuiAnchors :: MonadReader (Askable im am) m => m (Anchors.GuiAnchors im am)
readGuiAnchors = Lens.view aGuiAnchors

mkPrejumpPosSaver :: (Monad im, Monad am) => ExprGuiM im am (am ())
mkPrejumpPosSaver =
    do
        preJumpsMkProp <- readGuiAnchors <&> Anchors.preJumps
        preJumpsProp <- preJumpsMkProp ^. Property.mkProperty & im
        cursor <- Lens.view GuiState.cursor
        Property.pureModify preJumpsProp ((cursor:) . take 19) & pure

resetDepth :: MonadReader (Askable im am) m => Int -> m r -> m r
resetDepth depth = Reader.local (aDepthLeft .~ depth)

advanceDepth :: MonadReader (Askable im am) m => (WithTextPos View -> m r) -> m r -> m r
advanceDepth f action =
    do
        depth <- Lens.view aDepthLeft
        if depth <= 0
            then mkErrorWidget >>= f
            else action & Reader.local (aDepthLeft -~ 1)
    where
        mkErrorWidget = TextView.makeLabel "..."

readMScopeId :: MonadReader (Askable im am) m => m (CurAndPrev (Maybe ScopeId))
readMScopeId = Lens.view aMScopeId

withLocalMScopeId ::
    MonadReader (Askable im am) m => CurAndPrev (Maybe ScopeId) -> m a -> m a
withLocalMScopeId mScopeId = Reader.local (aMScopeId .~ mScopeId)

instance (Monad im, Semigroup a) => Semigroup (ExprGuiM im am a) where
    (<>) = liftA2 (<>)

instance (Monad im, Monoid a) => Monoid (ExprGuiM im am a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance MonadTransaction n im => MonadTransaction n (ExprGuiM im am) where
    transaction = im . transaction

makeSubexpression ::
    Monad im =>
    Sugar.Expression (Name am) im am ExprGui.Payload ->
    ExprGuiM im am (Responsive.Responsive (am GuiState.Update))
makeSubexpression expr =
    do
        maker <- Lens.view aMakeSubexpression
        maker expr
    & advanceDepth (pure . Responsive.fromTextView)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = expr ^. Sugar.rPayload & WidgetIds.fromExprPayload & toAnimId

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
