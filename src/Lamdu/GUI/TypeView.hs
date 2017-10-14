{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Lamdu.GUI.TypeView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (StateT, state, evalStateT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Map as Map
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/), hbox)
import           GUI.Momentu.View (R, View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.OrderTags (orderedFlatComposite)
import           System.Random (Random, random)
import qualified System.Random as Random

import           Lamdu.Prelude

newtype Prec = Prec Int deriving (Eq, Ord, Show)

newtype M m a = M
    { runM :: StateT Random.StdGen m a
    } deriving
    (Functor, Applicative, Monad, MonadTrans, MonadTransaction n, MonadReader r)

rand :: (Random r, Monad m) => M m r
rand = M $ state random

split :: Monad m => M m a -> M m a
split (M act) =
    do
        splitGen <- M $ state Random.split
        M $ lift $ evalStateT act splitGen

randAnimId :: Monad m => M m AnimId
randAnimId = WidgetId.toAnimId . WidgetIds.fromUUID <$> rand

text :: (MonadReader env m, TextView.HasStyle env) => Text -> M m (WithTextPos View)
text str =
    do
        animId <- randAnimId
        TextView.make ?? Text.replace "\0" "" str ?? animId

showIdentifier ::
    (MonadReader env m, TextView.HasStyle env) =>
    Identifier -> M m (WithTextPos View)
showIdentifier (Identifier bs) = text (decodeUtf8 bs)

parensAround ::
    (MonadReader env m, TextView.HasStyle env) =>
    WithTextPos View -> M m (WithTextPos View)
parensAround view =
    do
        openParenView <- text "("
        closeParenView <- text ")"
        pure $ hbox [openParenView, view, closeParenView]

parens ::
    (MonadReader env m, TextView.HasStyle env) =>
    Prec -> Prec -> WithTextPos View -> M m (WithTextPos View)
parens parent my view
    | parent > my = parensAround view
    | otherwise = pure view

makeTVar ::
    (MonadReader env m, TextView.HasStyle env) =>
    T.Var p -> M m (WithTextPos View)
makeTVar (T.Var name) = showIdentifier name

makeTFun ::
    ( MonadReader env m, TextView.HasStyle env, HasTheme env
    , Spacer.HasStdSpacing env, MonadTransaction n m
    ) =>
    Prec -> Type -> Type -> M m (WithTextPos View)
makeTFun parentPrecedence a b =
    case a of
    T.TRecord T.CEmpty -> [text "| "]
    _ ->
        [ splitMake (Prec 1) a
        , text " → "
        ]
    ++ [splitMake (Prec 0) b]
    & sequence
    <&> hbox
    >>= parens parentPrecedence (Prec 0)

makeTInst ::
    ( MonadReader env m, TextView.HasStyle env, Spacer.HasStdSpacing env
    , HasTheme env, MonadTransaction n m
    ) => Prec -> T.NominalId -> Map T.ParamId Type ->
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
                    typeView <- splitMake (Prec 0) arg
                    pure
                        [ toAligned 1 paramIdView
                        , Aligned 0.5 hspace
                        , toAligned 0 typeView
                        ]
        case Map.toList typeParams of
            [] -> pure nameView
            [(_, arg)] ->
                splitMake (Prec 0) arg
                <&> afterName
                >>= parens parentPrecedence (Prec 0)
            params ->
                mapM makeTypeParam params
                <&> GridView.make
                >>= addValPadding
                >>= addBGColor
                <&> afterName

toAligned :: SizedElement a => R -> WithTextPos a -> Aligned a
toAligned x (WithTextPos y w) =
    Aligned (Vector2 x (y / w ^. Element.height)) w

addValPadding :: (Element a, MonadReader env m, HasTheme env) => a -> M m a
addValPadding view =
    do
        padding <- Lens.view Theme.theme <&> Theme.valFramePadding <&> fmap realToFrac
        Element.pad padding view & pure

addBGColor :: (Element a, MonadReader env m, HasTheme env) => a -> M m a
addBGColor view =
    do
        color <- Lens.view Theme.theme <&> Theme.typeFrameBGColor
        bgId <- randAnimId
        view
            & MDraw.backgroundColor bgId color
            & pure

makeEmptyRecord :: (MonadReader env m, TextView.HasStyle env) => M m (WithTextPos View)
makeEmptyRecord = text "Ø"

makeTag ::
    (MonadTransaction n m, MonadReader env m, TextView.HasStyle env) =>
    T.Tag -> M m (WithTextPos View)
makeTag tag =
    Anchors.assocNameRef tag & Transaction.getP & transaction
    >>= text

makeField ::
    ( MonadTransaction n m, MonadReader env m, TextView.HasStyle env
    , HasTheme env, Spacer.HasStdSpacing env
    ) => (T.Tag, Type) -> M m [Aligned View]
makeField (tag, fieldType) =
    sequence
    [ makeTag tag <&> toAligned 1
    , Spacer.stdHSpace <&> Aligned 0.5
    , splitMake (Prec 0) fieldType <&> toAligned 0
    ]

makeSumField ::
    ( MonadReader env m, TextView.HasStyle env, Spacer.HasStdSpacing env
    , MonadTransaction n m, HasTheme env
    ) => (T.Tag, Type) -> M m [Aligned View]
makeSumField (tag, T.TRecord T.CEmpty) =
    makeTag tag <&> toAligned 1 <&> (:[])
makeSumField (tag, fieldType) = makeField (tag, fieldType)

makeComposite ::
    (MonadReader env m, TextView.HasStyle env, HasTheme env) =>
    ((T.Tag, Type) -> M m [Aligned View]) -> T.Composite t -> M m (WithTextPos View)
makeComposite _ T.CEmpty = makeEmptyRecord
makeComposite mkField composite =
    do
        fieldsView <- GridView.make <$> mapM mkField fields
        let barWidth
                | null fields = 150
                | otherwise = fieldsView ^. Element.width
        varView <-
            case extension of
            Nothing -> pure Element.empty
            Just var ->
                do
                    sqrId <- randAnimId
                    let sqr =
                            View.unitSquare sqrId
                            & Element.scale (Vector2 barWidth 10)
                            & Aligned 0.5
                    makeTVar var <&> (^. Align.tValue) <&> Aligned 0.5 <&> (sqr /-/)
        (Aligned 0.5 fieldsView /-/ varView) ^. Align.value & Align.WithTextPos 0
            & addValPadding
            >>= addBGColor
    where
        (fields, extension) = composite ^. orderedFlatComposite

splitMake ::
    ( MonadReader env m, TextView.HasStyle env, HasTheme env
    , Spacer.HasStdSpacing env, MonadTransaction n m
    ) => Prec -> Type -> M m (WithTextPos View)
splitMake parentPrecedence typ = split $ makeInternal parentPrecedence typ

makeInternal ::
    ( MonadReader env m, TextView.HasStyle env, Spacer.HasStdSpacing env
    , HasTheme env, MonadTransaction n m
    ) => Prec -> Type -> M m (WithTextPos View)
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

make ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , MonadTransaction n m
    ) =>
    Type -> AnimId -> m (WithTextPos View)
make t prefix =
    do
        typeTint <- Lens.view Theme.theme <&> Theme.typeTint
        makeInternal (Prec 0) t
            & runM
            & (`evalStateT` Random.mkStdGen 0)
            <&> Element.setLayers . Element.layers . Lens.mapped %~ Anim.mapIdentities (mappend prefix)
            <&> Element.tint typeTint
