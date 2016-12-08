{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lamdu.GUI.TypeView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (StateT, state, evalStateT)
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.Precedence (ParentPrecedence(..), MyPrecedence(..), needParens)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.OrderTags (orderedFlatComposite)
import           System.Random (Random, random)
import qualified System.Random as Random

import           Lamdu.Prelude

type T = Transaction
newtype M m a = M
    { runM :: StateT Random.StdGen (ExprGuiM m) a
    } deriving (Functor, Applicative, Monad)
egui :: Monad m => ExprGuiM m a -> M m a
egui = M . lift
transaction :: Monad m => T m a -> M m a
transaction = egui . ExprGuiM.transaction
rand :: (Random r, Monad m) => M m r
rand = M $ state random

wenv :: Monad m => WidgetEnvT (T m) a -> M m a
wenv = egui . ExprGuiM.widgetEnv

split :: Monad m => M m a -> M m a
split (M act) =
    do
        splitGen <- M $ state Random.split
        M $ lift $ evalStateT act splitGen

randAnimId :: Monad m => M m AnimId
randAnimId = WidgetId.toAnimId . WidgetIds.fromUUID <$> rand

text :: Monad m => Text -> M m View
text str = wenv . BWidgets.makeTextView str =<< randAnimId

showIdentifier :: Monad m => Identifier -> M m View
showIdentifier (Identifier bs) = text (decodeUtf8 bs)

hbox :: [View] -> View
hbox = GridView.horizontalAlign 0.5

mkHSpace :: Monad m => M m View
mkHSpace = ExprGuiM.widgetEnv BWidgets.stdHSpaceView & egui

parensAround :: Monad m => View -> M m View
parensAround view =
    do
        openParenView <- text "("
        closeParenView <- text ")"
        return $ hbox [openParenView, view, closeParenView]

parens ::
    Monad m => ParentPrecedence -> MyPrecedence -> View -> M m View
parens parent my view
    | needParens parent my = parensAround view
    | otherwise = return view

makeTVar :: Monad m => T.Var p -> M m View
makeTVar (T.Var name) = showIdentifier name

makeTFun :: Monad m => ParentPrecedence -> Type -> Type -> M m View
makeTFun parentPrecedence a b =
    case a of
    T.TRecord T.CEmpty -> [text "◗ "]
    _ ->
        [ splitMake (ParentPrecedence 1) a
        , text " → "
        ]
    ++ [splitMake (ParentPrecedence 0) b]
    & sequence
    <&> hbox
    >>= parens parentPrecedence (MyPrecedence 0)

makeTInst :: Monad m => ParentPrecedence -> T.NominalId -> Map T.ParamId Type -> M m View
makeTInst _parentPrecedence tid typeParams =
    do
        nameView <-
            Anchors.assocNameRef tid & Transaction.getP & transaction >>= text
        hspace <- mkHSpace
        let afterName paramsView = addValPadding $ hbox [nameView, hspace, paramsView]
            makeTypeParam (T.ParamId tParamId, arg) =
                do
                    paramIdView <- showIdentifier tParamId
                    typeView <- splitMake (ParentPrecedence 0) arg
                    return
                        [ (GridView.Alignment (Vector2 1 0.5), paramIdView)
                        , (0.5, hspace)
                        , (GridView.Alignment (Vector2 0 0.5), typeView)
                        ]
        case Map.toList typeParams of
            [] -> pure nameView
            [(_, arg)] ->
                splitMake (ParentPrecedence 0) arg
                >>= afterName
            params ->
                mapM makeTypeParam params
                <&> GridView.make
                >>= addBackgroundFrame
                >>= afterName

addValPadding :: Monad m => View -> M m View
addValPadding view =
    do
        config <- egui ExprGuiM.readConfig
        let padding = realToFrac <$> Config.valFramePadding config
        View.pad padding view & return

addBGColor :: Monad m => View -> M m View
addBGColor view =
    do
        config <- egui ExprGuiM.readConfig
        let color = Config.typeFrameBGColor config
        bgId <- randAnimId
        view
            & View.backgroundColor bgId color
            & return

addBackgroundFrame :: Monad m => View -> M m View
addBackgroundFrame v = v & addValPadding >>= addBGColor

makeEmptyRecord :: Monad m => M m View
makeEmptyRecord = text "Ø"

makeTag :: Monad m => T.Tag -> M m View
makeTag tag =
    Anchors.assocNameRef tag & Transaction.getP & transaction
    >>= text

makeField :: Monad m => (T.Tag, Type) -> M m [(GridView.Alignment, View)]
makeField (tag, fieldType) =
    Lens.sequenceOf (Lens.traversed . _2)
    [ (GridView.Alignment (Vector2 1 0.5), makeTag tag)
    , (0.5, mkHSpace)
    , (GridView.Alignment (Vector2 0 0.5), splitMake (ParentPrecedence 0) fieldType)
    ]

makeSumField :: Monad m => (T.Tag, Type) -> M m [(GridView.Alignment, View)]
makeSumField (tag, T.TRecord T.CEmpty) =
    makeTag tag <&> (,) (GridView.Alignment (Vector2 1 0.5)) <&> (:[])
makeSumField (tag, fieldType) =
    Lens.sequenceOf (Lens.traversed . _2)
    [ (GridView.Alignment (Vector2 1 0.5), makeTag tag)
    , (0.5, mkHSpace)
    , (GridView.Alignment (Vector2 0 0.5), splitMake (ParentPrecedence 0) fieldType)
    ]

makeComposite ::
    Monad m =>
    ((T.Tag, Type) -> M m [(GridView.Alignment, View)]) -> T.Composite t -> M m View
makeComposite _ T.CEmpty = makeEmptyRecord
makeComposite mkField composite =
    do
        fieldsView <- GridView.make <$> mapM mkField fields
        let barWidth
                | null fields = 150
                | otherwise = fieldsView ^. View.width
        varView <-
            case extension of
            Nothing -> pure View.empty
            Just var ->
                do
                    sqrId <- randAnimId
                    let sqr =
                            View.make 1 (Anim.unitSquare sqrId)
                            & View.scale (Vector2 barWidth 10)
                    v <- makeTVar var
                    return $ GridView.verticalAlign 0.5 [sqr, v]
        GridView.verticalAlign 0.5 [fieldsView, varView] & addBackgroundFrame
    where
        (fields, extension) = composite ^. orderedFlatComposite

splitMake :: Monad m => ParentPrecedence -> Type -> M m View
splitMake parentPrecedence typ = split $ makeInternal parentPrecedence typ

makeInternal :: Monad m => ParentPrecedence -> Type -> M m View
makeInternal parentPrecedence typ =
    case typ of
    T.TVar var -> makeTVar var
    T.TFun a b -> makeTFun parentPrecedence a b
    T.TInst typeId typeParams -> makeTInst parentPrecedence typeId typeParams
    T.TRecord composite -> makeComposite makeField composite
    T.TSum composite ->
        [ text "+"
        , makeComposite makeSumField composite
        ] & sequenceA
        <&> hbox

make :: Monad m => Type -> AnimId -> ExprGuiM m View
make t prefix =
    do
        config <- ExprGuiM.readConfig
        makeInternal (ParentPrecedence 0) t
            & runM
            & (`evalStateT` Random.mkStdGen 0)
            <&> View.animFrames %~ Anim.mapIdentities (mappend prefix)
            <&> View.tint (Config.typeTint config)
