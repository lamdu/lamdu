{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.GUI.TypeView
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT, state, evalStateT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.View (View)
import Lamdu.Expr.Identifier (Identifier(..))
import Lamdu.Expr.Type (Type)
import Lamdu.GUI.Precedence (ParentPrecedence(..), MyPrecedence(..))
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import System.Random (Random, random)
import qualified Data.ByteString.Char8 as BS8
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified System.Random as Random

showIdentifier :: Identifier -> String
showIdentifier (Identifier bs) = BS8.unpack bs

type T = Transaction
newtype M m a = M
  { runM :: StateT Random.StdGen (WidgetEnvT (T m)) a
  } deriving (Functor, Applicative, Monad)
wenv :: Monad m => WidgetEnvT (T m) a -> M m a
wenv = M . lift
rand :: (Random r, Monad m) => M m r
rand = M $ state random
split :: Monad m => M m a -> M m a
split (M act) =
  do
    splitGen <- M $ state Random.split
    M $ lift $ evalStateT act splitGen

randAnimId :: MonadA m => M m AnimId
randAnimId = WidgetId.toAnimId . WidgetIds.fromGuid <$> rand

text :: MonadA m => String -> M m View
text str = wenv . BWidgets.makeTextView str =<< randAnimId

hbox :: [View] -> View
hbox = GridView.horizontalAlign 0.5

parensAround :: MonadA m => View -> M m View
parensAround view =
  do
    openParenView <- text "("
    closeParenView <- text ")"
    return $ hbox [openParenView, view, closeParenView]

parens ::
  MonadA m => ParentPrecedence -> MyPrecedence -> View -> M m View
parens (ParentPrecedence parent) (MyPrecedence my) view
  | parent > my = parensAround view
  | otherwise = return view

makeTVar :: MonadA m => T.Var p -> M m View
makeTVar (T.Var name) = text (showIdentifier name)

makeTFun :: MonadA m => ParentPrecedence -> Type -> Type -> M m View
makeTFun parentPrecedence a b =
  parens parentPrecedence (MyPrecedence 0) =<<
  do
    aView <- splitMake (ParentPrecedence 1) a
    arrow <- text " -> "
    bView <- splitMake (ParentPrecedence 0) b
    return $ hbox [aView, arrow, bView]

makeTInst :: MonadA m => ParentPrecedence -> T.Id -> Map T.ParamId Type -> M m View
makeTInst parentPrecedence typeId typeParams = text "TODO"

makeRecord :: MonadA m => T.Composite T.Product -> M m View
makeRecord composite = text "TODO"

splitMake :: MonadA m => ParentPrecedence -> Type -> M m View
splitMake parentPrecedence typ = split $ makeInternal parentPrecedence typ

makeInternal :: MonadA m => ParentPrecedence -> Type -> M m View
makeInternal parentPrecedence typ =
  case typ of
  T.TVar var -> makeTVar var
  T.TFun a b -> makeTFun parentPrecedence a b
  T.TInst typeId typeParams -> makeTInst parentPrecedence typeId typeParams
  T.TRecord composite -> makeRecord composite

make :: MonadA m => Random.StdGen -> Type -> WidgetEnvT (T m) View
make gen = (`evalStateT` gen) . runM . makeInternal (ParentPrecedence 0)
