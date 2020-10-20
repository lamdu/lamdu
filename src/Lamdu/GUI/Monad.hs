{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE UndecidableInstances, PolymorphicComponents #-}
module Lamdu.GUI.Monad
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

    , makeSubexpression, makeBinder
    , assocTagName

    , GuiM, run
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Monoid as Monoid
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import           Data.Vector.Vector2 (Vector2)
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.State (GUIState(..))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget.Id (toAnimId)
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (ScopeId, topLevelScopeId)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.Name (Name)
import           Lamdu.Settings (Settings)
import           Lamdu.Style (Style, HasStyle)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving newtype (Semigroup, Monoid)

data Askable env i o = Askable
    { _aState :: GUIState
    , _aTextEditStyle :: TextEdit.Style
    , _aStdSpacing :: Vector2 Double
    , _aAnimIdPrefix :: AnimId
    , _aSettings :: Settings
    , _aConfig :: Config
    , _aTheme :: Theme
    , _aAssocTagName :: T.Tag -> MkProperty' o Text
    , _aMakeSubexpression ::
        Sugar.Expr Sugar.Term (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
        GuiM env i o (Responsive o)
    , _aMakeBinder ::
        Sugar.Expr Sugar.Binder (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
        GuiM env i o (Responsive o)
    , _aGuiAnchors :: Anchors.GuiAnchors i o
    , _aDepthLeft :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aStyle :: Style
    , _aIsHoleResult :: Bool
    , _aDirLayout :: Dir.Layout
    , _aEnv :: env
        -- TODO: This ^^ defeats the purpose and means ALL gui depends
        -- on ALL texts Need to parameterize GUI monad on the env, and
        -- each subcomponent in gui should put Has constraints on the
        -- env
    }

newtype GuiM env i o a =
    GuiM (ReaderT (Askable env i o) i a)
    deriving newtype (Functor, Applicative, Monad, MonadReader (Askable env i o))
    deriving (Semigroup, Monoid) via (Monoid.Ap (GuiM env i o) a)

Lens.makeLenses ''Askable

instance GuiState.HasCursor (Askable env i o)
instance Has GUIState (Askable env i o) where has = aState
instance Has TextView.Style (Askable env i o) where has = aTextEditStyle . has
instance Has TextEdit.Style (Askable env i o) where has = aTextEditStyle
instance Spacer.HasStdSpacing (Askable env i o) where stdSpacing = aStdSpacing
instance Element.HasAnimIdPrefix (Askable env i o) where animIdPrefix = aAnimIdPrefix
instance Has Config (Askable env i o) where has = aConfig
instance Has Theme (Askable env i o) where has = aTheme
instance Has ResponsiveExpr.Style (Askable env i o) where has = aTheme . has
instance Has Menu.Config (Askable env i o) where
    has = Menu.configLens (aConfig . Config.menu) (aTheme . Theme.menu)
instance Has SearchMenu.TermStyle (Askable env i o) where
    has = aTheme . Theme.searchTerm
instance Has Hover.Style (Askable env i o) where has = aTheme . has
instance Has Style (Askable env i o) where has = aStyle
instance Has Settings (Askable env i o) where has = aSettings
instance Has Dir.Layout (Askable env i o) where has = aDirLayout
instance Has LangId env => Has LangId (Askable env i o) where has = aEnv . has
instance Has (f Text) env => Has (f Text) (Askable env i o) where has = aEnv . has

im :: Monad i => i a -> GuiM env i o a
im = GuiM . lift

readGuiAnchors :: MonadReader (Askable env i o) m => m (Anchors.GuiAnchors i o)
readGuiAnchors = Lens.view aGuiAnchors

assocTagName :: MonadReader (Askable env i o) m => m (T.Tag -> MkProperty' o Text)
assocTagName = Lens.view aAssocTagName

mkPrejumpPosSaver :: (Monad i, Monad o) => GuiM env i o (o ())
mkPrejumpPosSaver =
    do
        preJumpsMkProp <- readGuiAnchors <&> Anchors.preJumps
        preJumpsProp <- preJumpsMkProp ^. Property.mkProperty & im
        cursor <- Lens.view GuiState.cursor
        Property.pureModify preJumpsProp ((cursor:) . take 19) & pure

resetDepth :: MonadReader (Askable env i o) m => Int -> m r -> m r
resetDepth depth = Reader.local (aDepthLeft .~ depth)

advanceDepth :: MonadReader (Askable env i o) m => (WithTextPos View -> m r) -> m r -> m r
advanceDepth f action =
    do
        depth <- Lens.view aDepthLeft
        if depth <= 0
            then mkErrorWidget >>= f
            else action & Reader.local (aDepthLeft -~ 1)
    where
        mkErrorWidget = Label.make "..."

readMScopeId :: MonadReader (Askable env i o) m => m (CurAndPrev (Maybe ScopeId))
readMScopeId = Lens.view aMScopeId

withLocalMScopeId ::
    MonadReader (Askable env i o) m => CurAndPrev (Maybe ScopeId) -> m a -> m a
withLocalMScopeId mScopeId = Reader.local (aMScopeId .~ mScopeId)

instance MonadTransaction n i => MonadTransaction n (GuiM env i o) where
    transaction = im . transaction

make ::
    Monad i =>
    Lens.Getter (Askable env i o)
        (Annotated (Sugar.Payload v name i o, a) # e -> GuiM env i o (Responsive.Responsive o)) ->
    Annotated (Sugar.Payload v name i o, a) # e ->
    GuiM env i o (Responsive.Responsive o)
make sub expr =
    do
        maker <- Lens.view sub
        maker expr
    & advanceDepth (pure . Responsive.fromTextView)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = expr ^. annotation . _1 & WidgetIds.fromExprPayload & toAnimId

makeSubexpression ::
    Monad i =>
    Sugar.Expr Sugar.Term (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive.Responsive o)
makeSubexpression = make aMakeSubexpression

makeBinder ::
    Monad i =>
    Sugar.Expr Sugar.Binder (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive.Responsive o)
makeBinder = make aMakeBinder

isHoleResult :: MonadReader (Askable env i o) m => m Bool
isHoleResult = Lens.view aIsHoleResult

withLocalIsHoleResult :: MonadReader (Askable env i o) m => m a -> m a
withLocalIsHoleResult = Reader.local (aIsHoleResult .~ True)

run ::
    ( GuiState.HasState env, Spacer.HasStdSpacing env, Has Dir.Layout env
    , Has Config env, Has Theme env
    , Has Settings env, HasStyle env
    ) =>
    (T.Tag -> MkProperty' o Text) ->
    (Sugar.Expr Sugar.Term (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
        GuiM env i o (Responsive o)) ->
    (Sugar.Expr Sugar.Binder (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
        GuiM env i o (Responsive o)) ->
    Anchors.GuiAnchors i o ->
    env -> GuiM env i o a -> i a
run assocTagName_ makeSubexpr mkBinder theGuiAnchors env (GuiM action) =
    runReaderT action
    Askable
    { _aAssocTagName = assocTagName_
    , _aState = env ^. has
    , _aTextEditStyle = env ^. has
    , _aStdSpacing = env ^. Spacer.stdSpacing
    , _aAnimIdPrefix = ["outermost"]
    , _aConfig = env ^. has
    , _aTheme = env ^. has
    , _aSettings = env ^. has
    , _aMakeSubexpression = makeSubexpr
    , _aMakeBinder = mkBinder
    , _aGuiAnchors = theGuiAnchors
    , _aDepthLeft = env ^. has . Config.maxExprDepth
    , _aMScopeId = Just topLevelScopeId & pure
    , _aStyle = env ^. has
    , _aIsHoleResult = False
    , _aDirLayout = env ^. has
    , _aEnv = env
    }
