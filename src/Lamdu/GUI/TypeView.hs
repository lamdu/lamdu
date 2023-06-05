{-# LANGUAGE TemplateHaskell, TupleSections, TypeFamilies #-}
module Lamdu.GUI.TypeView
    ( make, makeScheme, addTypeBG
    ) where

import qualified Control.Lens as Lens
import           Data.Bitraversable (Bitraversable(..))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import           GUI.Momentu (Aligned(..), WithTextPos(..), View, (/|/))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
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
        Glue.hbox [openParenView, view, closeParenView] Dir.LeftToRight & pure

parens :: _ => Prec -> Prec -> WithTextPos View -> m (WithTextPos View)
parens parent my view
    | parent > my = parensAround view
    | otherwise = pure view

makeTFun ::
    ( MonadReader env m, Has Dir.Layout env, Element.HasElemIdPrefix env, Has Theme env
    , Spacer.HasStdSpacing env, Has (Texts.Name Text) env, Has (Texts.Code Text) env
    ) =>
    Prec ->
    Annotated Sugar.EntityId # Sugar.Type Name ->
    Annotated Sugar.EntityId # Sugar.Type Name ->
    m (WithTextPos View)
makeTFun parentPrecedence a b =
    case a ^. hVal of
    Sugar.TRecord (Sugar.CompositeFields [] Nothing) ->
        [ grammar "|"
        , Spacer.stdHSpace <&> WithTextPos 0
        ]
    _ ->
        [ makeInternal (Prec 1) a
        , Styled.grammar (Styled.label Texts.arrow)
        ]
    <> [makeInternal (Prec 0) b]
    & sequence
    >>= Glue.hbox >>= parens parentPrecedence (Prec 0)

makeTInst ::
    (MonadReader env m, _) =>
    Prec -> Sugar.TId Name ->
    [(Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name)] ->
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
                traverse makeTypeParam params
                >>= gridViewTopLeftAlign
                <&> Align.toWithTextPos
                >>= Styled.addValPadding
                >>= addTypeBG
                & afterName
    where
        tconsName =
            NameView.make (tid ^. Sugar.tidName) & disambElemId ["TCons"]
        disambElemId suffixes =
            local (Element.elemIdPrefix <>~ M.ElemId (suffixes <&> BS8.pack))

addTypeBG :: _ => a -> m a
addTypeBG view =
    do
        color <- Lens.view (has . Theme.typeFrameBGColor)
        MDraw.backgroundColor color view

makeEmptyComposite :: _ => m (WithTextPos View)
makeEmptyComposite = grammar "Ø"

makeField ::
    _ =>
    (Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name) ->
    m (WithTextPos View, WithTextPos View)
makeField = bitraverse TagView.make (makeInternal (Prec 0))

makeVariantField ::
    _ =>
    (Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name) ->
    m (WithTextPos View, WithTextPos View)
makeVariantField (tag, Ann _ (Sugar.TRecord (Sugar.CompositeFields [] Nothing))) =
    TagView.make tag <&> (, Element.empty)
makeVariantField t = makeField t

gridViewTopLeftAlign :: _ => vert (horiz (Aligned View)) -> m (Aligned View)
gridViewTopLeftAlign views =
    GridView.make views
    <&>
    \(alignPoints, view) ->
    case alignPoints ^? traverse . traverse of
    Nothing -> Aligned 0 view
    Just x -> x & Align.value .~ view

makeComposite ::
    _ =>
    m (WithTextPos View) -> m (WithTextPos View) ->
    ((Sugar.Tag Name, Annotated Sugar.EntityId # Sugar.Type Name) ->
         m (WithTextPos View, WithTextPos View)) ->
    Sugar.CompositeFields Name # Annotated Sugar.EntityId ->
    m (WithTextPos View)
makeComposite mkOpener mkCloser mkField composite =
    case composite of
    Sugar.CompositeFields [] Nothing -> makeEmptyComposite
    Sugar.CompositeFields [] (Just var) ->
        makeExt var
        & crPre .~ Styled.grammar mkOpener
        & crPost .~ Styled.grammar mkCloser
        & addElemIdPrefix (0 :: Int)
        & sequenceA
        >>= gridViewTopLeftAlign . (:[]) . horizSetCompositeRow
        <&> Align.toWithTextPos
    Sugar.CompositeFields fields extension ->
        traverse mkField fields
        <&> map toRow
        <&> Lens.ix 0 . crPre .~ Styled.grammar mkOpener
        <&> addExt
        <&> Lens._last . crPost .~ Styled.grammar mkCloser
        <&> Lens.imap addElemIdPrefix
        >>= traverse sequenceA
        <&> map horizSetCompositeRow
        >>= gridViewTopLeftAlign
        >>= Styled.addValPadding
        <&> Align.alignmentRatio . _1 .~ 0.5
        >>= Styled.addValFrame
        <&> Align.toWithTextPos
        where
            addExt =
                case extension of
                Nothing -> id
                Just var -> (<> [makeExt var]) . (Lens._last . crPost .~ pure Element.empty)
    where
        makeExt var =
            CompositeRow (pure Element.empty)
            (Styled.grammar (Styled.label Texts.compositeExtendTail))
            (pure Element.empty) (NameView.make var) (pure Element.empty)
        addElemIdPrefix i row = row <&> Element.locallyAugmented i
        toRow (t, v) =
            CompositeRow (pure Element.empty) (pure t) space (pure v)
            (Styled.grammar (Styled.label Texts.compositeSeparator))
            where
                space
                    | v ^. Align.tValue . Element.width == 0 = pure Element.empty
                    | otherwise = Spacer.stdHSpace <&> WithTextPos 0

makeInternal :: _ => Prec -> Annotated Sugar.EntityId # Sugar.Type Name -> m (WithTextPos View)
makeInternal parentPrecedence (Ann (Const entityId) tbody) =
    case tbody of
    Sugar.TVar var -> NameView.make var
    Sugar.TFun (FuncType a b) -> makeTFun parentPrecedence a b
    Sugar.TInst typeId typeParams -> makeTInst parentPrecedence typeId typeParams
    Sugar.TRecord composite ->
        makeComposite (Styled.label Texts.recordOpener) (Styled.label Texts.recordCloser)
        makeField composite
    Sugar.TVariant composite ->
        makeComposite (Styled.label Texts.caseOpener) (Styled.label Texts.caseCloser)
        makeVariantField composite
    & local (Element.elemIdPrefix .~ elemId)
    where
        elemId = WidgetIds.fromEntityId entityId & M.asElemId

make :: _ => Annotated Sugar.EntityId # Sugar.Type Name -> m (WithTextPos View)
make t = makeInternal (Prec 0) t & Styled.withColor TextColors.typeTextColor

makeScheme :: _ => Sugar.Scheme Name -> m (WithTextPos View)
makeScheme s = make (s ^. Sugar.schemeType)
