{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, PolymorphicComponents #-}
{-# LANGUAGE DerivingVia  #-}
module Lamdu.GUI.ExpressionGui.Monad
    ( StoredEntityIds(..)
    --
    , advanceDepth, resetDepth
    --
    , mkPrejumpPosSaver
    --
    , readMScopeId, withLocalMScopeId
    --
    , isHoleResult, withLocalIsHoleResult
    --
    , im
    , IOM(..), iom

    , makeSubexpression, makeBinder

    , ExprGuiM, run
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev)
import           Data.Has (Has(..))
import qualified Data.Monoid as Monoid
import qualified Data.Property as Property
import           Data.Vector.Vector2 (Vector2)
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.State (Gui, GUIState(..))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget.Id (toAnimId)
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme, HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (ScopeId, topLevelScopeId)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Language (Language, language)
import qualified Lamdu.I18N.Language as Language
import           Lamdu.Name (Name, NameTexts)
import           Lamdu.Settings (Settings, HasSettings(..))
import           Lamdu.Style (Style, HasStyle(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving newtype (Semigroup, Monoid)

data Askable i o = Askable
    { _aState :: GUIState
    , _aTextEditStyle :: TextEdit.Style
    , _aStdSpacing :: Vector2 Double
    , _aAnimIdPrefix :: AnimId
    , _aSettings :: Settings
    , _aConfig :: Config
    , _aTheme :: Theme
    , _aMakeSubexpression :: ExprGui.SugarExpr i o -> ExprGuiM i o (Gui Responsive o)
    , _aMakeBinder ::
        Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
        (Sugar.Binder (Name o) i o) ->
        ExprGuiM i o (Gui Responsive o)
    , _aGuiAnchors :: Anchors.GuiAnchors i o
    , _aDepthLeft :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aStyle :: Style
    , _aIsHoleResult :: Bool
    , _aDirLayout :: Dir.Layout
    , aIom :: forall x. i x -> o x
    , _aLanguage :: Language
    }

newtype ExprGuiM i (o :: * -> *) a =
    ExprGuiM (ReaderT (Askable i o) i a)
    deriving newtype (Functor, Applicative, Monad, MonadReader (Askable i o))
    deriving (Semigroup, Monoid) via (Monoid.Ap (ExprGuiM i o) a)

Lens.makeLenses ''Askable

instance GuiState.HasCursor (Askable i o)
instance Has GUIState (Askable i o) where has = aState
instance Has TextView.Style (Askable i o) where has = aTextEditStyle . has
instance Has TextEdit.Style (Askable i o) where has = aTextEditStyle
instance Spacer.HasStdSpacing (Askable i o) where stdSpacing = aStdSpacing
instance Element.HasAnimIdPrefix (Askable i o) where animIdPrefix = aAnimIdPrefix
instance Config.HasConfig (Askable i o) where config = aConfig
instance HasTheme (Askable i o) where theme = aTheme
instance Has ResponsiveExpr.Style (Askable i o) where has = aTheme . has
instance Menu.HasConfig (Askable i o) where
    config = Menu.configLens (aConfig . Config.menu) (aTheme . Theme.menu)
instance Has SearchMenu.TermStyle (Askable i o) where
    has = aTheme . Theme.searchTerm
instance Has Hover.Style (Askable i o) where has = aTheme . has
instance HasStyle (Askable i o) where style = aStyle
instance HasSettings (Askable i o) where settings = aSettings
instance Has Dir.Layout (Askable i o) where has = aDirLayout
instance Dir.HasTexts (Askable i o) where texts = language . Dir.texts
instance EventMap.HasTexts (Askable i o) where texts = language . EventMap.texts
instance Glue.HasTexts (Askable i o) where texts = language . Glue.texts
instance Menu.HasTexts (Askable i o) where texts = language . Menu.texts
instance SearchMenu.HasTexts (Askable i o) where texts = language . SearchMenu.texts
instance Grid.HasTexts (Askable i o) where texts = language . Grid.texts
instance Choice.HasTexts (Askable i o) where texts = language . Choice.texts
instance TextEdit.HasTexts (Askable i o) where texts = language . TextEdit.texts
instance Has (NameTexts Text) (Askable i o) where has = language . has
instance Has LangId (Askable i o) where has = language . has
instance Language.HasLanguage (Askable i o) where language = aLanguage

im :: Monad i => i a -> ExprGuiM i o a
im = ExprGuiM . lift

newtype IOM i o = IOM (forall x. i x -> o x)
iom :: Monad i => ExprGuiM i o (IOM i o)
iom = Lens.view id <&> \askable -> IOM (aIom askable)

readGuiAnchors :: MonadReader (Askable i o) m => m (Anchors.GuiAnchors i o)
readGuiAnchors = Lens.view aGuiAnchors

mkPrejumpPosSaver :: (Monad i, Monad o) => ExprGuiM i o (o ())
mkPrejumpPosSaver =
    do
        preJumpsMkProp <- readGuiAnchors <&> Anchors.preJumps
        preJumpsProp <- preJumpsMkProp ^. Property.mkProperty & im
        cursor <- Lens.view GuiState.cursor
        Property.pureModify preJumpsProp ((cursor:) . take 19) & pure

resetDepth :: MonadReader (Askable i o) m => Int -> m r -> m r
resetDepth depth = Reader.local (aDepthLeft .~ depth)

advanceDepth :: MonadReader (Askable i o) m => (WithTextPos View -> m r) -> m r -> m r
advanceDepth f action =
    do
        depth <- Lens.view aDepthLeft
        if depth <= 0
            then mkErrorWidget >>= f
            else action & Reader.local (aDepthLeft -~ 1)
    where
        mkErrorWidget = Label.make "..."

readMScopeId :: MonadReader (Askable i o) m => m (CurAndPrev (Maybe ScopeId))
readMScopeId = Lens.view aMScopeId

withLocalMScopeId ::
    MonadReader (Askable i o) m => CurAndPrev (Maybe ScopeId) -> m a -> m a
withLocalMScopeId mScopeId = Reader.local (aMScopeId .~ mScopeId)

instance MonadTransaction n i => MonadTransaction n (ExprGuiM i o) where
    transaction = im . transaction

make ::
    Monad i =>
    Lens.Getter (Askable i o)
        (Ann (Sugar.Payload name i o a) e -> ExprGuiM i o (Gui Responsive.Responsive o)) ->
    Ann (Sugar.Payload name i o a) e ->
    ExprGuiM i o (Gui Responsive.Responsive o)
make sub expr =
    do
        maker <- Lens.view sub
        maker expr
    & advanceDepth (pure . Responsive.fromTextView)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = expr ^. ann & WidgetIds.fromExprPayload & toAnimId

makeSubexpression ::
    Monad i =>
    Sugar.Expression (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    ExprGuiM i o (Gui Responsive.Responsive o)
makeSubexpression = make aMakeSubexpression

makeBinder ::
    Monad i =>
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) (Sugar.Binder (Name o) i o) ->
    ExprGuiM i o (Gui Responsive.Responsive o)
makeBinder = make aMakeBinder

isHoleResult :: MonadReader (Askable i o) m => m Bool
isHoleResult = Lens.view aIsHoleResult

withLocalIsHoleResult :: MonadReader (Askable i o) m => m a -> m a
withLocalIsHoleResult = Reader.local (aIsHoleResult .~ True)

run ::
    ( GuiState.HasState env, Spacer.HasStdSpacing env
    , Config.HasConfig env, HasTheme env
    , HasSettings env, HasStyle env, Language.HasLanguage env
    ) =>
    (ExprGui.SugarExpr i o -> ExprGuiM i o (Gui Responsive o)) ->
    (Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
        (Sugar.Binder (Name o) i o)
        -> ExprGuiM i o (Gui Responsive o)) ->
    Anchors.GuiAnchors i o ->
    env -> (forall x. i x -> o x) -> ExprGuiM i o a -> i a
run makeSubexpr mkBinder theGuiAnchors env liftIom (ExprGuiM action) =
    runReaderT action
    Askable
    { _aState = env ^. has
    , _aTextEditStyle = env ^. has
    , _aStdSpacing = env ^. Spacer.stdSpacing
    , _aAnimIdPrefix = ["outermost"]
    , _aConfig = env ^. Config.config
    , _aTheme = env ^. Theme.theme
    , _aSettings = env ^. settings
    , _aMakeSubexpression = makeSubexpr
    , _aMakeBinder = mkBinder
    , _aGuiAnchors = theGuiAnchors
    , _aDepthLeft = env ^. Config.config . Config.maxExprDepth
    , _aMScopeId = Just topLevelScopeId & pure
    , _aStyle = env ^. style
    , _aIsHoleResult = False
    , _aDirLayout = env ^. has
    , _aLanguage = env ^. language
    , aIom = liftIom
    }
