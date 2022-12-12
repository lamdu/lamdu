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
    , im

    , makeSubexpression, makeBinder
    , openPane

    , GuiM, _GuiM, run
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..), MonadReader(..))
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Monoid as Monoid
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import           GUI.Momentu (Responsive, View, WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget.Id (toAnimId)
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (ScopeId, topLevelScopeId)
import qualified Lamdu.GUI.Classes as C
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype StoredEntityIds = StoredEntityIds [Sugar.EntityId]
    deriving newtype (Semigroup, Monoid)

data Askable env i o = Askable
    { _aAssocTagName :: T.Tag -> MkProperty' o Text
    , _aMakeSubexpression :: ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive o)
    , _aMakeBinder :: ExprGui.Expr Sugar.Binder i o -> GuiM env i o (Responsive o)
    , _aGoto :: Sugar.GotoDest -> o Sugar.EntityId
    , _aGuiAnchors :: Anchors.GuiAnchors i o
    , _aDepthLeft :: Int
    , _aMScopeId :: CurAndPrev (Maybe ScopeId)
    , _aEnv :: env
    }

newtype GuiM env i o a =
    GuiM (ReaderT (Askable env i o) i a)
    deriving newtype (Functor, Applicative, Monad)
    deriving (Semigroup, Monoid) via (Monoid.Ap (GuiM env i o) a)

Lens.makeLenses ''Askable
Lens.makePrisms ''GuiM

instance Monad i => MonadReader env (GuiM env i o) where
    ask = GuiM (Lens.view aEnv)
    local = (_GuiM %~) . Lens.locally aEnv

instance Monad i => C.InfoMonad (GuiM env i o) i where liftInfo = im

instance (Monad i, Monad o) => C.SetTagName (GuiM env i o) o where
    setTagName = GuiM (Lens.view aAssocTagName) <&> Lens.mapped %~ Property.setP

im :: Monad i => i a -> GuiM env i o a
im = GuiM . lift

readGuiAnchors :: Monad i => GuiM env i o (Anchors.GuiAnchors i o)
readGuiAnchors = GuiM (Lens.view aGuiAnchors)

openPane :: Monad i => GuiM env i o (Sugar.GotoDest -> o Sugar.EntityId)
openPane = GuiM (Lens.view aGoto)

mkPrejumpPosSaver :: _ => GuiM env i o (o ())
mkPrejumpPosSaver =
    do
        preJumpsMkProp <- readGuiAnchors <&> Anchors.preJumps
        preJumpsProp <- preJumpsMkProp ^. Property.mkProperty & im
        cursor <- Lens.view GuiState.cursor
        Property.pureModify preJumpsProp ((cursor:) . take 19) & pure

advanceDepth :: _ => (WithTextPos View -> GuiM env i o r) -> GuiM env i o r -> GuiM env i o r
advanceDepth f action =
    do
        depth <- GuiM (Lens.view aDepthLeft)
        if depth <= 0
            then mkErrorWidget >>= f
            else action & _GuiM %~ local (aDepthLeft -~ 1)
    where
        mkErrorWidget = Label.make "..."

resetDepth :: _ => GuiM env i o r -> GuiM env i o r
resetDepth action =
    do
        depth <- Lens.view (Config.hasConfig . Config.maxExprDepth)
        action & _GuiM %~ local (aDepthLeft .~ depth)

readMScopeId :: Monad i => GuiM env i o (CurAndPrev (Maybe ScopeId))
readMScopeId = GuiM (Lens.view aMScopeId)

withLocalMScopeId :: Monad i => CurAndPrev (Maybe ScopeId) -> GuiM env i o a -> GuiM env i o a
withLocalMScopeId mScopeId = _GuiM %~ local (aMScopeId .~ mScopeId)

instance MonadTransaction n i => MonadTransaction n (GuiM env i o) where
    transaction = im . transaction

make ::
    _ =>
    Lens.Getter (Askable env i o)
        (Annotated (Sugar.Payload v o) # e -> GuiM env i o (Responsive.Responsive o)) ->
    Annotated (Sugar.Payload v o) # e ->
    GuiM env i o (Responsive.Responsive o)
make sub expr =
    do
        maker <- GuiM (Lens.view sub)
        maker expr
    & advanceDepth (pure . Responsive.fromTextView)
    & local (Element.animIdPrefix .~ animId)
    where
        animId = expr ^. annotation & WidgetIds.fromExprPayload & toAnimId

makeSubexpression :: _ => ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive.Responsive o)
makeSubexpression = make aMakeSubexpression

makeBinder :: _ => ExprGui.Expr Sugar.Binder i o -> GuiM env i o (Responsive.Responsive o)
makeBinder = make aMakeBinder

run ::
    _ =>
    (Sugar.GotoDest -> o Sugar.EntityId) ->
    (T.Tag -> MkProperty' o Text) ->
    (ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive o)) ->
    (ExprGui.Expr Sugar.Binder i o -> GuiM env i o (Responsive o)) ->
    Anchors.GuiAnchors i o ->
    env -> GuiM env i o a -> i a
run goto assocTagName_ makeSubexpr mkBinder theGuiAnchors env (GuiM action) =
    runReaderT action
    Askable
    { _aAssocTagName = assocTagName_
    , _aMakeSubexpression = makeSubexpr
    , _aMakeBinder = mkBinder
    , _aGuiAnchors = theGuiAnchors
    , _aDepthLeft = env ^. Config.hasConfig . Config.maxExprDepth
    , _aMScopeId = Just topLevelScopeId & pure
    , _aGoto = goto
    , _aEnv = env
    }
