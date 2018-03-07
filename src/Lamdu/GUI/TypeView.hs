{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Lamdu.GUI.TypeView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import           Control.Monad.State (StateT, state, evalStateT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Transaction (MonadTransaction(..), getP)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/), hbox)
import           GUI.Momentu.View (View(..))
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
import qualified Lamdu.GUI.Styled as Styled
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

grammar ::
    (MonadReader env m, TextView.HasStyle env, HasTheme env) =>
    Text -> M m (WithTextPos View)
grammar str =
    do
        animId <- randAnimId
        Styled.grammarText ?? Text.replace "\0" "" str ?? animId

showIdentifier ::
    (MonadReader env m, TextView.HasStyle env) =>
    Identifier -> M m (WithTextPos View)
showIdentifier (Identifier bs) = text (decodeUtf8 bs)

parensAround ::
    (MonadReader env m, TextView.HasStyle env, HasTheme env) =>
    WithTextPos View -> M m (WithTextPos View)
parensAround view =
    do
        openParenView <- grammar "("
        closeParenView <- grammar ")"
        hbox [openParenView, view, closeParenView] & pure

parens ::
    (MonadReader env m, TextView.HasStyle env, HasTheme env) =>
    Prec -> Prec -> WithTextPos View -> M m (WithTextPos View)
parens parent my view
    | parent > my = parensAround view
    | otherwise = pure view

makeTVar ::
    (MonadReader env m, TextView.HasStyle env) =>
    T.Var p -> M m (WithTextPos View)
makeTVar (T.Var name) = showIdentifier name

makeTFun ::
    (MonadReader env m, HasTheme env, Spacer.HasStdSpacing env, MonadTransaction n m) =>
    Prec -> Type -> Type -> M m (WithTextPos View)
makeTFun parentPrecedence a b =
    case a of
    T.TRecord T.CEmpty -> [grammar "| "]
    _ ->
        [ splitMake (Prec 1) a
        , grammar " → "
        ]
    ++ [splitMake (Prec 0) b]
    & sequence
    <&> hbox
    >>= parens parentPrecedence (Prec 0)

makeTInst ::
    (MonadReader env m, Spacer.HasStdSpacing env, HasTheme env, MonadTransaction n m) =>
    Prec -> T.NominalId -> Map T.ParamId Type -> M m (WithTextPos View)
makeTInst parentPrecedence tid typeParams =
    do
        tag <- Anchors.assocTag tid & getP
        nameView <- Anchors.assocTagNameRef tag & getP <&> Lens.ix 0 %~ Char.toUpper >>= text
        hspace <- Spacer.stdHSpace
        let afterName paramsView = nameView /|/ hspace /|/ paramsView
        let makeTypeParam (T.ParamId tParamId, arg) =
                do
                    paramIdView <- showIdentifier tParamId
                    typeView <- splitMake (Prec 0) arg
                    pure
                        [ Align.fromWithTextPos 1 paramIdView
                        , Aligned 0.5 hspace
                        , Align.fromWithTextPos 0 typeView
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
                >>= (Styled.addValPadding ??)
                >>= addTypeBG
                <&> afterName

addTypeBG :: (Element a, MonadReader env m, HasTheme env) => a -> M m a
addTypeBG view =
    do
        color <- Lens.view Theme.theme <&> Theme.typeFrameBGColor
        bgId <- randAnimId
        view
            & MDraw.backgroundColor bgId color
            & pure

makeEmptyComposite ::
    (MonadReader env m, TextView.HasStyle env, HasTheme env) =>
    M m (WithTextPos View)
makeEmptyComposite = grammar "Ø"

makeTag ::
    (MonadTransaction n m, MonadReader env m, TextView.HasStyle env) =>
    T.Tag -> M m (WithTextPos View)
makeTag tag =
    Anchors.assocTagNameRef tag & getP
    <&> Lens.filtered Text.null .~ "(empty)"
    >>= text

makeField ::
    (MonadTransaction n m, MonadReader env m, HasTheme env, Spacer.HasStdSpacing env) =>
    (T.Tag, Type) -> M m [Aligned View]
makeField (tag, fieldType) =
    sequence
    [ makeTag tag <&> Align.fromWithTextPos 1
    , Spacer.stdHSpace <&> Aligned 0.5
    , splitMake (Prec 0) fieldType <&> Align.fromWithTextPos 0
    ]

makeVariantField ::
    (MonadReader env m, Spacer.HasStdSpacing env, MonadTransaction n m, HasTheme env) =>
    (T.Tag, Type) -> M m [Aligned View]
makeVariantField (tag, T.TRecord T.CEmpty) =
    makeTag tag <&> Align.fromWithTextPos 1 <&> (:[])
    -- ^ Nullary data constructor
makeVariantField (tag, fieldType) = makeField (tag, fieldType)

makeComposite ::
    (MonadReader env m, TextView.HasStyle env, HasTheme env) =>
    Text -> Text ->
    M m (Aligned View) -> ((T.Tag, Type) -> M m [Aligned View]) -> T.Composite t -> M m (WithTextPos View)
makeComposite _ _ _ _ T.CEmpty = makeEmptyComposite
makeComposite o c sepView mkField composite =
    do
        opener <- grammar o <&> (^. Align.tValue)
        closer <- grammar c <&> (^. Align.tValue)
        rawFieldsView <-
            traverse mkField fields
            >>= zipWithM prepend (pure Element.empty : repeat sepView)
            <&> GridView.make
        let openedFieldsView :: View
            openedFieldsView =
                (Aligned 0 opener /|/ Aligned 0 rawFieldsView) ^. Align.value
        let fieldsView =
                (Aligned 1 openedFieldsView /|/ Aligned 1 closer) ^. Align.value
        let barWidth
                | null fields = 150
                | otherwise = fieldsView ^. Element.width
        extView <-
            case extension of
            Nothing -> pure Element.empty
            Just var ->
                do
                    sqrId <- randAnimId
                    let sqr =
                            View.unitSquare sqrId
                            & Element.scale (Vector2 barWidth 10)
                    sep <- sepView
                    varView <- makeTVar var <&> Align.fromWithTextPos 0
                    let lastLine = (sep /|/ varView) ^. Align.value
                    pure (Aligned 0.5 sqr /-/ Aligned 0.5 lastLine)
        (Aligned 0.5 fieldsView /-/ extView) ^. Align.value & Align.WithTextPos 0
            & (Styled.addValPadding ??)
            >>= addTypeBG
    where
        prepend sep field = (:) <$> sep ?? field
        (fields, extension) = composite ^. orderedFlatComposite

splitMake ::
    (MonadReader env m, HasTheme env, Spacer.HasStdSpacing env, MonadTransaction n m) =>
    Prec -> Type -> M m (WithTextPos View)
splitMake parentPrecedence typ = split $ makeInternal parentPrecedence typ

makeInternal ::
    (MonadReader env m, Spacer.HasStdSpacing env, HasTheme env, MonadTransaction n m) =>
    Prec -> Type -> M m (WithTextPos View)
makeInternal parentPrecedence typ =
    case typ of
    T.TVar var -> makeTVar var
    T.TFun a b -> makeTFun parentPrecedence a b
    T.TInst typeId typeParams -> makeTInst parentPrecedence typeId typeParams
    T.TRecord composite -> makeComposite "{" "}" (pure Element.empty) makeField composite
    T.TVariant composite ->
        [ makeComposite "+{" "}" (grammar "or: " <&> Align.fromWithTextPos 0) makeVariantField composite
        ] & sequenceA
        <&> hbox

make ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , Element.HasAnimIdPrefix env, MonadTransaction n m
    ) =>
    Type -> m (WithTextPos View)
make t =
    do
        prefix <- Lens.view Element.animIdPrefix
        makeInternal (Prec 0) t
            & runM
            & (`evalStateT` Random.mkStdGen 0)
            <&> Element.setLayers . Element.layers . Lens.mapped %~ Anim.mapIdentities (mappend prefix)
