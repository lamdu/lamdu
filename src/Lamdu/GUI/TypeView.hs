{-# LANGUAGE TemplateHaskell, TupleSections, TypeFamilies #-}
module Lamdu.GUI.TypeView
    ( make, makeScheme, addTypeBG
    ) where

import qualified Control.Lens as Lens
import           Data.Bitraversable (Bitraversable(..))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Hyper.Syntax (FuncType(..))
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype Prec = Prec Int deriving stock (Eq, Ord, Show)

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

grammar :: _ => Text -> m (WithTextPos View)
grammar = Styled.grammar . Label.make . sanitize

parensAround :: _ => WithTextPos View -> m (WithTextPos View)
parensAround view =
    do
        openParenView <- grammar "("
        closeParenView <- grammar ")"
        Glue.hbox Dir.LeftToRight [openParenView, view, closeParenView] & pure

parens :: _ => Prec -> Prec -> WithTextPos View -> m (WithTextPos View)
parens parent my view
    | parent > my = parensAround view
    | otherwise = pure view

makeTFun ::
    ( MonadReader env m, Has Dir.Layout env, Element.HasAnimIdPrefix env, Has Theme env
    , Spacer.HasStdSpacing env, Has (Texts.Name Text) env, Has (Texts.Code Text) env
    ) =>
    Prec ->
    Annotated Sugar.EntityId # Sugar.Type Name o ->
    Annotated Sugar.EntityId # Sugar.Type Name o ->
    m (WithTextPos View)
makeTFun parentPrecedence a b =
    Glue.hbox <*>
    ( case a ^. hVal of
        Sugar.TRecord (Sugar.CompositeFields [] Nothing) ->
            [ grammar "|"
            , Spacer.stdHSpace <&> WithTextPos 0
            ]
        _ ->
            [ makeInternal (Prec 1) a
            , Styled.grammar (Styled.label Texts.arrow)
            ]
        ++ [makeInternal (Prec 0) b]
        & sequence
    ) >>= parens parentPrecedence (Prec 0)

makeTInst ::
    (MonadReader env m, _) =>
    Prec -> Sugar.TId Name o ->
    [(Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name o)] ->
    m (WithTextPos View)
makeTInst parentPrecedence tid typeParams =
    do
        hspace <- Spacer.stdHSpace
        let afterName paramsView = tconsName /|/ pure hspace /|/ paramsView
        let makeTypeParam (tParamId, arg) =
                do
                    paramIdView <- TagView.make tParamId
                    typeView <- makeInternal (Prec 0) arg
                    pure
                        [ Align.fromWithTextPos 1 paramIdView
                        , Aligned 0.5 hspace
                        , Align.fromWithTextPos 0 typeView
                        ]
        case typeParams of
            [] -> tconsName
            [(_, arg)] ->
                makeInternal (Prec 0) arg
                & afterName
                >>= parens parentPrecedence (Prec 0)
            params ->
                gridViewTopLeftAlign <*> traverse makeTypeParam params
                <&> Align.toWithTextPos
                >>= (Styled.addValPadding ??)
                >>= addTypeBG
                & afterName
    where
        tconsName =
            NameView.make (tid ^. Sugar.tidName) & disambAnimId ["TCons"]
        disambAnimId suffixes =
            local (Element.animIdPrefix <>~ (suffixes <&> BS8.pack))

addTypeBG :: _ => a -> m a
addTypeBG view =
    do
        color <- Lens.view (has . Theme.typeFrameBGColor)
        bgId <- Element.subAnimId ?? ["bg"]
        view
            & MDraw.backgroundColor bgId color
            & pure

makeEmptyComposite :: _ => m (WithTextPos View)
makeEmptyComposite = grammar "Ø"

makeField ::
    _ =>
    (Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name o) ->
    m (WithTextPos View, WithTextPos View)
makeField = bitraverse TagView.make (makeInternal (Prec 0))

makeVariantField ::
    _ =>
    (Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name o) ->
    m (WithTextPos View, WithTextPos View)
makeVariantField (tag, Ann _ (Sugar.TRecord (Sugar.CompositeFields [] Nothing))) =
    TagView.make tag <&> (, Element.empty)
makeVariantField t = makeField t

gridViewTopLeftAlign :: _ => m (vert (horiz (Aligned View)) -> Aligned View)
gridViewTopLeftAlign =
    GridView.make <&>
    \mkGrid views ->
    let (alignPoints, view) = mkGrid views
    in  case alignPoints ^? traverse . traverse of
        Nothing -> Aligned 0 view
        Just x -> x & Align.value .~ view

makeComposite ::
    _ =>
    m (WithTextPos View) -> m (WithTextPos View) -> m (WithTextPos View) ->
    ((Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name o) ->
         m (WithTextPos View, WithTextPos View)) ->
    Sugar.CompositeFields Name (Annotated Sugar.EntityId # Sugar.Type Name o) ->
    m (WithTextPos View)
makeComposite mkOpener mkPre mkPost mkField composite =
    case composite of
    Sugar.CompositeFields [] Nothing -> makeEmptyComposite
    Sugar.CompositeFields fields extension ->
        Styled.addValFrame <*>
        do
            opener <- Styled.grammar mkOpener
            closer <- Styled.label Texts.recordCloser & Styled.grammar
            fieldsView <-
                gridViewTopLeftAlign <*>
                ( traverse mkField fields
                <&> map toRow
                <&> Lens.ix 0 . crPre .~ pure opener
                <&> Lens.reversed . Lens.ix 0 . crPost .~ pure closer
                <&> Lens.imap addAnimIdPrefix
                >>= traverse sequenceA
                <&> map horizSetCompositeRow )
                <&> Align.alignmentRatio . _1 .~ 0.5
            let barWidth
                    | null fields = 150
                    | otherwise = fieldsView ^. Element.width
            extView <-
                case extension of
                Nothing -> pure Element.empty
                Just var ->
                    do
                        sqrId <- Element.subAnimId ?? ["square"]
                        let sqr =
                                View.unitSquare sqrId
                                & Element.scale (Vector2 barWidth 10)
                        lastLine <- mkPre /|/ NameView.make var <&> (^. Align.tValue)
                        pure (Aligned 0.5 sqr) /-/ pure (Aligned 0.5 lastLine)
            Styled.addValPadding
                <*> (pure fieldsView /-/ pure extView <&> Align.toWithTextPos)
    where
        addAnimIdPrefix i row = row <&> Element.locallyAugmented i
        toRow (t, v) =
            CompositeRow mkPre (pure t) space (pure v) mkPost
            where
                space
                    | v ^. Align.tValue . Element.width == 0 = pure Element.empty
                    | otherwise = Spacer.stdHSpace <&> WithTextPos 0

makeInternal :: _ => Prec -> Annotated Sugar.EntityId # Sugar.Type Name o -> m (WithTextPos View)
makeInternal parentPrecedence (Ann (Const entityId) tbody) =
    case tbody of
    Sugar.TVar var -> NameView.make var
    Sugar.TFun (FuncType a b) -> makeTFun parentPrecedence a b
    Sugar.TInst typeId typeParams -> makeTInst parentPrecedence typeId typeParams
    Sugar.TRecord composite ->
        makeComposite (Styled.label Texts.recordOpener)
        (pure Element.empty) (Styled.grammar (Styled.label Texts.recordSep))
        makeField composite
    Sugar.TVariant composite ->
        makeComposite (Styled.label Texts.variantTypeOpener)
        (Styled.grammar (Styled.label Texts.variantTypeSep)) (pure Element.empty)
        makeVariantField composite
    & local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId

make :: _ => Annotated Sugar.EntityId # Sugar.Type Name o -> m (WithTextPos View)
make t = makeInternal (Prec 0) t & Styled.withColor TextColors.typeTextColor

makeScheme :: _ => Sugar.Scheme Name o -> m (WithTextPos View)
makeScheme s = make (s ^. Sugar.schemeType)
