{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TemplateHaskell #-}
module Lamdu.GUI.TypeView
    ( make, makeScheme
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/), hbox)
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.ExpressionEdit.TagEdit (makeTagView)
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype Prec = Prec Int deriving (Eq, Ord, Show)

data CompositeRow a = CompositeRow
    { _crPre :: a
    , _crTag :: a
    , _crSpace :: a
    , _crVal :: a
    , _crPost :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''CompositeRow

horizSetCompositeRow :: CompositeRow (WithTextPos View) -> CompositeRow (Aligned View)
horizSetCompositeRow r =
    CompositeRow
    { _crPre = r ^. crPre & Align.fromWithTextPos 0
    , _crTag = r ^. crTag & Align.fromWithTextPos 1
    , _crSpace = r ^. crSpace & Align.fromWithTextPos 0.5
    , _crVal = r ^. crVal & Align.fromWithTextPos 0
    , _crPost = r ^. crPost & Align.fromWithTextPos 0
    }

sanitize :: Text -> Text
sanitize = Text.replace "\0" ""

text ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Text -> m (WithTextPos View)
text = TextView.makeLabel . sanitize

grammar ::
    ( MonadReader env m, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) =>
    Text -> m (WithTextPos View)
grammar = Styled.grammarLabel . sanitize

showIdentifier ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Identifier -> m (WithTextPos View)
showIdentifier (Identifier bs) = text (decodeUtf8 bs)

parensAround ::
    ( MonadReader env m, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) =>
    WithTextPos View -> m (WithTextPos View)
parensAround view =
    do
        openParenView <- grammar "("
        closeParenView <- grammar ")"
        hbox [openParenView, view, closeParenView] & pure

parens ::
    ( MonadReader env m, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) =>
    Prec -> Prec -> WithTextPos View -> m (WithTextPos View)
parens parent my view
    | parent > my = parensAround view
    | otherwise = pure view

makeTVar ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Sugar.TVar p -> m (WithTextPos View)
makeTVar (T.Var name) = showIdentifier name

makeTFun ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , Element.HasAnimIdPrefix env
    ) =>
    Prec -> Sugar.Type (Name f) -> Sugar.Type (Name f) -> m (WithTextPos View)
makeTFun parentPrecedence a b =
    case a ^. Sugar.tBody of
    Sugar.TRecord (Sugar.CompositeFields [] Nothing) -> [grammar "| "]
    _ ->
        [ makeInternal (Prec 1) a
        , grammar " → "
        ]
    ++ [makeInternal (Prec 0) b]
    & sequence
    <&> hbox
    >>= parens parentPrecedence (Prec 0)

makeTInst ::
    ( MonadReader env m, Spacer.HasStdSpacing env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) =>
    Prec -> Sugar.TId (Name f) -> Map Sugar.ParamId (Sugar.Type (Name f)) ->
    m (WithTextPos View)
makeTInst parentPrecedence tid typeParams =
    do
        nameView <- NameView.make (tid ^. Sugar.tidName)
        hspace <- Spacer.stdHSpace
        let afterName paramsView = nameView /|/ hspace /|/ paramsView
        let makeTypeParam (T.ParamId tParamId, arg) =
                do
                    paramIdView <- showIdentifier tParamId
                    typeView <- makeInternal (Prec 0) arg
                    pure
                        [ Align.fromWithTextPos 1 paramIdView
                        , Aligned 0.5 hspace
                        , Align.fromWithTextPos 0 typeView
                        ]
        case Map.toList typeParams of
            [] -> pure nameView
            [(_, arg)] ->
                makeInternal (Prec 0) arg
                <&> afterName
                >>= parens parentPrecedence (Prec 0)
            params ->
                mapM makeTypeParam params
                <&> gridViewTopLeftAlign
                <&> Align.toWithTextPos
                >>= (Styled.addValPadding ??)
                >>= addTypeBG
                <&> afterName

addTypeBG ::
    (Element a, MonadReader env m, HasTheme env, Element.HasAnimIdPrefix env) =>
    a -> m a
addTypeBG view =
    do
        color <- Lens.view (Theme.theme . Theme.typeFrameBGColor)
        bgId <- Element.subAnimId ["bg"]
        view
            & MDraw.backgroundColor bgId color
            & pure

makeEmptyComposite ::
    ( MonadReader env m, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) =>
    m (WithTextPos View)
makeEmptyComposite = grammar "Ø"

makeField ::
    ( MonadReader env m, HasTheme env
    , Spacer.HasStdSpacing env, Element.HasAnimIdPrefix env
    ) =>
    (Sugar.TagInfo (Name f), Sugar.Type (Name f)) ->
    m (WithTextPos View, WithTextPos View)
makeField (tag, fieldType) =
    (,)
    <$> makeTagView tag
    <*> makeInternal (Prec 0) fieldType

makeVariantField ::
    ( MonadReader env m, Spacer.HasStdSpacing env
    , HasTheme env, Element.HasAnimIdPrefix env
    ) =>
    (Sugar.TagInfo (Name f), Sugar.Type (Name f)) ->
    m (WithTextPos View, WithTextPos View)
makeVariantField (tag, Sugar.Type _ (Sugar.TRecord (Sugar.CompositeFields [] Nothing))) =
    makeTagView tag <&> flip (,) Element.empty
    -- ^ Nullary data constructor
makeVariantField (tag, fieldType) = makeField (tag, fieldType)

gridViewTopLeftAlign ::
    (Traversable vert, Traversable horiz) =>
    vert (horiz (Aligned View)) -> Aligned View
gridViewTopLeftAlign views =
    case alignPoints ^? traverse . traverse of
    Nothing -> Aligned 0 view
    Just x -> x & Align.value .~ view
    where
        (alignPoints, view) = GridView.make views

makeComposite ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , Element.HasAnimIdPrefix env
    ) =>
    Text -> Text ->
    m (WithTextPos View) -> m (WithTextPos View) ->
    ((Sugar.TagInfo (Name f), Sugar.Type (Name f)) ->
         m (WithTextPos View, WithTextPos View)) ->
    Sugar.CompositeFields t (Name f) (Sugar.Type (Name f)) ->
    m (WithTextPos View)
makeComposite o c mkPre mkPost mkField composite =
    case composite of
    Sugar.CompositeFields [] Nothing -> makeEmptyComposite
    Sugar.CompositeFields fields extension ->
        do
            opener <- grammar o
            closer <- grammar c
            fieldsView <-
                traverse mkField fields
                <&> map toRow
                <&> Lens.ix 0 . crPre .~ pure opener
                <&> Lens.reversed . Lens.ix 0 . crPost .~ pure closer
                >>= traverse sequenceA
                <&> map horizSetCompositeRow
                <&> gridViewTopLeftAlign
                <&> Align.alignmentRatio . _1 .~ 0.5
            let barWidth
                    | null fields = 150
                    | otherwise = fieldsView ^. Element.width
            extView <-
                case extension of
                Nothing -> pure Element.empty
                Just var ->
                    do
                        sqrId <- Element.subAnimId ["square"]
                        let sqr =
                                View.unitSquare sqrId
                                & Element.scale (Vector2 barWidth 10)
                        varView <- makeTVar var
                        pre <- mkPre
                        let lastLine = (pre /|/ varView) ^. Align.tValue
                        pure (Aligned 0.5 sqr /-/ Aligned 0.5 lastLine)
            fieldsView /-/ extView & Align.toWithTextPos
                & (Styled.addValPadding ??)
                >>= addTypeBG
    where
        toRow (t, v) =
            CompositeRow mkPre (pure t) space (pure v) mkPost
            where
                space
                    | v ^. Align.tValue . Element.width == 0 = pure Element.empty
                    | otherwise = Spacer.stdHSpace <&> WithTextPos 0

makeInternal ::
    ( MonadReader env m, Spacer.HasStdSpacing env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) =>
    Prec -> Sugar.Type (Name f) -> m (WithTextPos View)
makeInternal parentPrecedence (Sugar.Type entityId tbody) =
    case tbody of
    Sugar.TVar var -> makeTVar var
    Sugar.TFun a b -> makeTFun parentPrecedence a b
    Sugar.TInst typeId typeParams -> makeTInst parentPrecedence typeId typeParams
    Sugar.TRecord composite -> makeComposite "{" "}" (pure Element.empty) (grammar ",") makeField composite
    Sugar.TVariant composite ->
        makeComposite "+{" "}" (grammar "or: ") (pure Element.empty) makeVariantField composite
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId

make ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , Element.HasAnimIdPrefix env
    ) =>
    Sugar.Type (Name f) -> m (WithTextPos View)
make t = makeInternal (Prec 0) t & Styled.withColor TextColors.typeTextColor

makeScheme ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , Element.HasAnimIdPrefix env
    ) =>
    Sugar.Scheme (Name f) -> m (WithTextPos View)
makeScheme s = make (s ^. Sugar.schemeType)
