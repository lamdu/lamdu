{-# LANGUAGE FlexibleContexts #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction)
import qualified Control.Monad.Transaction as Transaction
import qualified Data.Binary.Utils as BinUtils
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Animation.Id as AnimId
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/), hbox, vbox)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionGui.Monad (MonadExprGui)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.Sugar.Types (ResVal)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

textView ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> m (WithTextPos View)
textView text = (TextView.make ?? text) <*> Lens.view Element.animIdPrefix

label ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> m (WithTextPos View)
label text =
    textView text & Reader.local (Element.animIdPrefix <>~ [encodeUtf8 text])

makeTag ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , MonadTransaction n m
    ) => T.Tag -> m (WithTextPos View)
makeTag tag =
    Anchors.assocTagNameRef tag & Transaction.getP
    <&> Lens.filtered Text.null .~ "(empty)"
    >>= textView
    -- TODO: animIdPrefix <>~ here?

makeField :: MonadExprGui m => T.Tag -> ResVal -> m [Aligned View]
makeField tag val =
    do
        tagView <- makeTag tag & Reader.local (Element.animIdPrefix <>~ ["tag"])
        space <- Spacer.stdHSpace
        valView <- makeInner val & Reader.local (Element.animIdPrefix <>~ ["val"])
        pure
            [ toAligned 1 tagView
            , Aligned 0.5 space
            , toAligned 0 valView
            ]
        & Reader.local (Element.animIdPrefix <>~ [BinUtils.encodeS tag])
    where
        toAligned x (WithTextPos y w) =
            Aligned (Vector2 x (y / w ^. Element.height)) w

makeError ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Sugar.EvalError -> m (WithTextPos View)
makeError err =
    textView msg & Reader.local (Element.animIdPrefix <>~ ["error"])
    where
        msg =
            case err of
            Sugar.EvalHole -> "?"
            _ -> Text.pack (show err)

arrayCutoff :: Int
arrayCutoff = 10

tableCutoff :: Int
tableCutoff = 6

makeTable :: MonadExprGui m => Sugar.ResTable ResVal -> m (WithTextPos View)
makeTable (Sugar.ResTable headers valss) =
    do
        header <- mapM makeHeader headers
        rows <- zipWithM makeRow [0..tableCutoff-1] valss
        s <- Spacer.stdHSpace
        let table =
                header : rows <&> traverse %~ (^. Align.tValue)
                <&> List.intersperse s
                <&> traverse %~ Aligned 0.5
                & GridView.make & snd & Align.WithTextPos 0
        remainView <-
            if null (drop tableCutoff rows)
            then pure Element.empty
            else label "…"
        Aligned 0.5 table /-/ Aligned 0.5 remainView ^. Align.value & pure
    where
        makeHeader tag =
            makeTag tag
            & Reader.local (Element.animIdPrefix <>~ [BinUtils.encodeS tag, "tag"])
        makeCell colI val =
            makeInner val
            & Reader.local (Element.animIdPrefix %~ (AnimId.augmentId ?? colI))
        makeRow rowI rowVals =
            zipWithM makeCell [(0::Int)..] rowVals
            & Reader.local (Element.animIdPrefix %~ (AnimId.augmentId ?? rowI))

makeArray :: MonadExprGui m => [ResVal] -> m (WithTextPos View)
makeArray items =
    do
        itemViews <- zipWith makeItem [0..arrayCutoff] items & sequence
        opener <- label "["
        closer <- label "]"
        opener : itemViews ++ [closer] & hbox & pure
    where
        makeItem idx val =
            [ [ label ", " | idx > 0 ]
            , [ makeInner val & Reader.local (Element.animIdPrefix <>~ ["val"])
                | idx < arrayCutoff ]
            , [ label "…" | idx == arrayCutoff ]
            ] & concat
            & sequence
            <&> hbox
            & Reader.local (Element.animIdPrefix %~ (Anim.augmentId ?? (idx :: Int)))

makeTree :: MonadExprGui m => Sugar.ResTree ResVal -> m (WithTextPos View)
makeTree (Sugar.ResTree root subtrees) =
    do
        rootView <-
            makeInner root & Reader.local (Element.animIdPrefix <>~ ["root"])
        subtreeViews <- zipWithM makeItem [0..cutoff] subtrees
        rootView : subtreeViews & vbox & pure
    where
        makeItem idx val =
            [ [ label "* " ]
            , [ makeInner val
                & Reader.local (Element.animIdPrefix <>~ ["val"])
                | idx < cutoff ]
            , [ label "…" | idx == cutoff ]
            ] & concat
            & sequence
            <&> hbox
            & Reader.local (Element.animIdPrefix %~ (Anim.augmentId ?? (idx :: Int)))
        cutoff = 4


makeRecord :: MonadExprGui m => Sugar.ResRecord ResVal -> m (WithTextPos View)
makeRecord (Sugar.ResRecord fields) =
    traverse (uncurry makeField) fields <&> GridView.make <&> snd
    <&> Align.WithTextPos 0

makeStream :: MonadExprGui m => Sugar.ResStream ResVal -> m (WithTextPos View)
makeStream (Sugar.ResStream head_) =
    do
        o <- label "["
        inner <- makeInner head_ & Reader.local (Element.animIdPrefix <>~ ["head"])
        c <- label ", …]" <&> (^. Align.tValue)
        o /|/ inner
            & Align.tValue %~ (hGlueAlign 1 ?? c)
            & pure
    where
        hGlueAlign align l r = (Aligned align l /|/ Aligned align r) ^. Align.value

makeInject :: MonadExprGui m => Sugar.ResInject ResVal -> m (WithTextPos View)
makeInject (Sugar.ResInject tag mVal) =
    do
        tagView <- makeTag tag & Reader.local (Element.animIdPrefix <>~ ["tag"])
        case mVal of
            Nothing -> pure tagView
            Just val ->
                do
                    s <- Spacer.stdHSpace
                    i <- makeInner val & Reader.local (Element.animIdPrefix <>~ ["val"])
                    tagView /|/ s /|/ i & pure

depthCounts :: Sugar.ResVal -> [Int]
depthCounts v =
    v ^.. Sugar.resBody . Lens.folded
    & take arrayCutoff
    <&> depthCounts
    & List.transpose
    <&> sum
    & (1 :)

-- Make animation frames of eval views animate from the whole rect
fixSize :: WithTextPos View -> WithTextPos View
fixSize view =
    view & Align.tValue . View.animFrames . Anim.frameImages . traverse %~ onImage
    where
        size = view ^. Element.size
        onImage image =
            image
            & Anim.iRect . Rect.size .~ size
            & Anim.iUnitImage %~
            (DrawUtils.scale (image ^. Anim.iRect . Rect.size / view ^. Element.size) %%)

makeInner :: MonadExprGui m => ResVal -> m (WithTextPos View)
makeInner (Sugar.ResVal body) =
    case body of
    Sugar.RError err -> makeError err
    Sugar.RFunc{} -> textView "Fn"
    Sugar.RInject inject -> makeInject inject
    Sugar.RRecord record -> makeRecord record
    Sugar.RText txt -> toText txt
    Sugar.RBytes x -> toText x
    Sugar.RFloat x -> toText x
    Sugar.RArray x -> makeArray x
    Sugar.RTree x -> makeTree x
    Sugar.RTable x -> makeTable x
    Sugar.RStream x -> makeStream x
    & advanceDepth
    where
        -- Only cut non-leaf expressions due to depth limits
        advanceDepth
            | Lens.has Lens.folded body = ExprGuiM.advanceDepth pure
            | otherwise = id

toText ::
    ( Format r, MonadReader env m, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) => r -> m (WithTextPos View)
toText val =
    textView cut
    where
        text = format val
        cut =
            map limLine start ++
            ( case rest of
              [] -> []
              _ -> ["…"]
            ) & Text.intercalate "\n"
            where
                (start, rest) = splitAt 10 (Text.lines text)
        limLine :: Text -> Text
        limLine ln =
            start <>
            if Text.null rest
            then ""
            else "…"
            where
                (start, rest) = Text.splitAt 100 ln

make :: MonadExprGui m => ResVal -> m (WithTextPos View)
make v =
    do
        maxEvalViewSize <- Lens.view (Theme.theme . Theme.maxEvalViewSize)
        let depthLimit =
                depthCounts v & scanl (+) 0 & tail
                & takeWhile (< maxEvalViewSize) & length
        makeInner v
            & ExprGuiM.resetDepth depthLimit
    <&> fixSize
