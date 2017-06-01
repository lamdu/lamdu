{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Lamdu.GUI.WidgetsEnvT
    ( WidgetEnvT, hoist, runWidgetEnvT
    , mapWidgetEnvT

    , Env(..), envCursor, envTextStyle

    , readEnv

    , textColor
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Transaction (MonadTransaction(..), Transaction)
import           Data.Vector.Vector2 (Vector2)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.GUI.Spacing as Spacing

import           Lamdu.Prelude

type T = Transaction

data Env = Env
    { _envCursor :: Widget.Id
    , _envTextStyle :: TextEdit.Style
    , _stdSpacing :: Vector2 Double
    }
Lens.makeLenses ''Env

instance Widget.HasCursor Env where cursor = envCursor
instance TextView.HasStyle Env where style = envTextStyle . TextView.style
instance TextEdit.HasStyle Env where style = envTextStyle
instance Spacing.HasStdSpacing Env where stdSpacing = stdSpacing

newtype WidgetEnvT m a = WidgetEnvT
    { _widgetEnvT :: ReaderT Env m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Env)
Lens.makeLenses ''WidgetEnvT

instance MonadTransaction n m => MonadTransaction n (WidgetEnvT m) where
    transaction = lift . transaction

runWidgetEnvT :: Env -> WidgetEnvT m a -> m a
runWidgetEnvT env (WidgetEnvT action) = runReaderT action env

hoist ::
    (MonadReader env m, Widget.HasCursor env,
     TextEdit.HasStyle env, Spacing.HasStdSpacing env,
     MonadTransaction n m) =>
    WidgetEnvT (T n) b -> m b
hoist action = do
    env <-
        Env
        <$> Lens.view Widget.cursor
        <*> Lens.view TextEdit.style
        <*> Lens.view Spacing.stdSpacing
    runWidgetEnvT env action & transaction

mapWidgetEnvT :: (m a -> n a) -> WidgetEnvT m a -> WidgetEnvT n a
mapWidgetEnvT = (widgetEnvT %~) . Reader.mapReaderT

readEnv :: Monad m => WidgetEnvT m Env
readEnv = WidgetEnvT Reader.ask

textColor :: Lens' Env Draw.Color
textColor = envTextStyle . TextEdit.sTextViewStyle . TextView.styleColor
