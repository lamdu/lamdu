{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Lamdu.GUI.TypeView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (StateT, state, evalStateT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Map as Map
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Align (Aligned(..), WithTextPos(..))
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (R, View(..), (/-/), (/|/))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget.Id as WidgetId
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.Precedence (ParentPrecedence(..), MyPrecedence(..), needParens)
import qualified Lamdu.GUI.Precedence as Precedence
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.OrderTags (orderedFlatComposite)
import           System.Random (Random, random)
import qualified System.Random as Random

import           Lamdu.Prelude

newtype M m a = M
    { runM :: StateT Random.StdGen (ExprGuiM m) a
    } deriving (Functor, Applicative, Monad, MonadReader (ExprGuiM.Askable m))
egui :: Monad m => ExprGuiM m a -> M m a
egui = M . lift
rand :: (Random r, Monad m) => M m r
rand = M $ state random

instance Monad m => MonadTransaction m (M m) where transaction = egui . transaction

split :: Monad m => M m a -> M m a
split (M act) =
    do
        splitGen <- M $ state Random.split
        M $ lift $ evalStateT act splitGen

randAnimId :: Monad m => M m AnimId
randAnimId = WidgetId.toAnimId . WidgetIds.fromUUID <$> rand

text :: Monad m => Text -> M m (WithTextPos View)
text str =
    do
        animId <- randAnimId
        TextView.make ?? Text.replace "\0" "" str ?? animId

showIdentifier :: Monad m => Identifier -> M m (WithTextPos View)
showIdentifier (Identifier bs) = text (decodeUtf8 bs)

parensAround :: Monad m => WithTextPos View -> M m (WithTextPos View)
parensAround view =
    do
        openParenView <- text "("
        closeParenView <- text ")"
        return $ View.hbox [openParenView, view, closeParenView]

parens ::
    Monad m => ParentPrecedence -> MyPrecedence ->
    WithTextPos View -> M m (WithTextPos View)
parens parent my view
    | needParens parent my = parensAround view
    | otherwise = return view

makeTVar :: Monad m => T.Var p -> M m (WithTextPos View)
makeTVar (T.Var name) = showIdentifier name

makeTFun :: Monad m => ParentPrecedence -> Type -> Type -> M m (WithTextPos View)
makeTFun parentPrecedence a b =
    case a of
    T.TRecord T.CEmpty -> [text "◗ "]
    _ ->
        [ splitMake (Precedence.parent 1) a
        , text " → "
        ]
    ++ [splitMake (Precedence.parent 0) b]
    & sequence
    <&> View.hbox
    >>= parens parentPrecedence (Precedence.my 0)

makeTInst ::
    Monad m => ParentPrecedence -> T.NominalId -> Map T.ParamId Type ->
    M m (WithTextPos View)
makeTInst parentPrecedence tid typeParams =
    do
        nameView <-
            Anchors.assocNameRef tid & Transaction.getP & transaction >>= text
        hspace <- Spacer.stdHSpace
        let afterName paramsView = nameView /|/ hspace /|/ paramsView
        let makeTypeParam (T.ParamId tParamId, arg) =
                do
                    paramIdView <- showIdentifier tParamId
                    typeView <- splitMake (Precedence.parent 0) arg
                    return
                        [ toAligned 1 paramIdView
                        , Aligned 0.5 hspace
                        , toAligned 0 typeView
                        ]
        case Map.toList typeParams of
            [] -> pure nameView
            [(_, arg)] ->
                splitMake (Precedence.parent 0) arg
                <&> afterName
                >>= parens parentPrecedence (Precedence.my 0)
            params ->
                mapM makeTypeParam params
                <&> GridView.make
                >>= addValPadding
                >>= addBGColor
                <&> afterName

toAligned :: View.HasSize a => R -> WithTextPos a -> Aligned a
toAligned x (WithTextPos y w) =
    Aligned (Vector2 x (y / w ^. View.height)) w

addValPadding :: (View.Resizable a, Monad m) => a -> M m a
addValPadding view =
    do
        padding <- Lens.view Theme.theme <&> Theme.valFramePadding <&> fmap realToFrac
        View.pad padding view & return

addBGColor :: (View.SetLayers a, Monad m) => a -> M m a
addBGColor view =
    do
        color <- Lens.view Theme.theme <&> Theme.typeFrameBGColor
        bgId <- randAnimId
        view
            & View.backgroundColor bgId color
            & return

makeEmptyRecord :: Monad m => M m (WithTextPos View)
makeEmptyRecord = text "Ø"

makeTag :: Monad m => T.Tag -> M m (WithTextPos View)
makeTag tag =
    Anchors.assocNameRef tag & Transaction.getP & transaction
    >>= text

makeField :: Monad m => (T.Tag, Type) -> M m [Aligned View]
makeField (tag, fieldType) =
    sequence
    [ makeTag tag <&> toAligned 1
    , Spacer.stdHSpace <&> Aligned 0.5
    , splitMake (Precedence.parent 0) fieldType <&> toAligned 0
    ]

makeSumField :: Monad m => (T.Tag, Type) -> M m [Aligned View]
makeSumField (tag, T.TRecord T.CEmpty) =
    makeTag tag <&> toAligned 1 <&> (:[])
makeSumField (tag, fieldType) = makeField (tag, fieldType)

makeComposite ::
    Monad m =>
    ((T.Tag, Type) -> M m [Aligned View]) -> T.Composite t -> M m (WithTextPos View)
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
                            & Aligned 0.5
                    makeTVar var <&> (^. Align.tValue) <&> Aligned 0.5 <&> (sqr /-/)
        (Aligned 0.5 fieldsView /-/ varView) ^. Align.value & Align.WithTextPos 0
            & addValPadding
            >>= addBGColor
    where
        (fields, extension) = composite ^. orderedFlatComposite

splitMake :: Monad m => ParentPrecedence -> Type -> M m (WithTextPos View)
splitMake parentPrecedence typ = split $ makeInternal parentPrecedence typ

makeInternal :: Monad m => ParentPrecedence -> Type -> M m (WithTextPos View)
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
        <&> View.hbox

make :: Monad m => Type -> AnimId -> ExprGuiM m (WithTextPos View)
make t prefix =
    do
        typeTint <- Lens.view Theme.theme <&> Theme.typeTint
        makeInternal (Precedence.parent 0) t
            & runM
            & (`evalStateT` Random.mkStdGen 0)
            <&> View.setLayers . View.layers . Lens.mapped %~ Anim.mapIdentities (mappend prefix)
            <&> View.tint typeTint
