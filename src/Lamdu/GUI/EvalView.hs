{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module Lamdu.GUI.EvalView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators.Extended as Draw
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Formatting (Format(..))
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import           Lamdu.Sugar.Types (ResVal)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Env env = Env
    { _depthLeft :: !Int
    , _base :: env
    }
Lens.makeLenses ''Env

instance Element.HasAnimIdPrefix env => Element.HasAnimIdPrefix (Env env) where
    animIdPrefix = base . Element.animIdPrefix
instance Spacer.HasStdSpacing env => Spacer.HasStdSpacing (Env env) where
    stdSpacing = base . Spacer.stdSpacing
instance Has r env => Has r (Env env) where has = base . has

type M env = ReaderT (Env env)

-- NOTE: We are hard-coded to GuiM because of the expression depth
-- ttl that eval results respect and change

textView ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, Has TextView.Style env
    , Has Dir.Layout env
    ) => Text -> m (WithTextPos View)
textView text = (TextView.make ?? text) <*> Lens.view Element.animIdPrefix

makeField :: _ => Sugar.Tag Name -> ResVal Name -> M env m [Aligned View]
makeField tag val =
    do
        tagView <- TagView.make tag
        space <- Spacer.stdHSpace
        valView <- makeInner val
        pure
            [ toAligned 1 tagView
            , Aligned 0.5 space
            , toAligned 0 valView
            ]
    where
        toAligned x (WithTextPos y w) =
            Aligned (Vector2 x (y / w ^. Element.height)) w

makeError ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, Has TextView.Style env
    , Has Dir.Layout env
    ) =>
    Sugar.EvalTypeError -> m (WithTextPos View)
makeError (Sugar.EvalTypeError msg) =
    textView msg & Reader.local (Element.animIdPrefix <>~ ["error"])

advanceDepth :: _ => M env m (WithTextPos View) -> M env m (WithTextPos View)
advanceDepth action =
    do
        depth <- Lens.view depthLeft
        if depth <= 0
            then Label.make "..."
            else action & Reader.local (depthLeft -~ 1)

arrayCutoff :: Int
arrayCutoff = 10

tableCutoff :: Int
tableCutoff = 6

makeTable :: _ => Sugar.ResTable Name (ResVal Name) -> M env m (WithTextPos View)
makeTable (Sugar.ResTable headers valss) =
    do
        header <- traverse TagView.make headers
        rows <- take (tableCutoff-1) valss & traverse . traverse %%~ makeInner
        s <- Spacer.stdHSpace
        table <-
            GridView.make ??
            ( header : rows <&> traverse %~ (^. Align.tValue)
                <&> List.intersperse s
                <&> traverse %~ Aligned 0.5
            ) <&> snd <&> Align.WithTextPos 0
        remainView <-
            if null (drop tableCutoff rows)
            then pure Element.empty
            else Label.make "…"
        pure (Aligned 0.5 table) /-/ pure (Aligned 0.5 remainView)
            <&> (^. Align.value)

makeArray :: _ => [ResVal Name] -> M env m (WithTextPos View)
makeArray items =
    do
        itemViews <- zipWithM makeItem [0..arrayCutoff] items
        (preLabel, postLabel) <-
            Lens.view has <&>
            \case
            Dir.LeftToRight -> ("[", "]")
            Dir.RightToLeft -> ("]", "[")
        opener <- Label.make preLabel
        closer <- Label.make postLabel
        Glue.hbox ?? opener : itemViews ++ [closer]
    where
        makeItem idx val =
            Glue.hbox
            <*> ( [ [ Label.make ", " | idx > 0 ]
                    , [ makeInner val
                      | idx < arrayCutoff ]
                    , [ Label.make "…" | idx == arrayCutoff ]
                    ] & concat & sequence
                )
            & Element.locallyAugmented (idx :: Int)

makeTree :: _ => Sugar.ResTree (ResVal Name) -> M env m (WithTextPos View)
makeTree (Sugar.ResTree root subtrees) =
    do
        rootView <- makeInner root
        subtreeViews <- zipWithM makeItem [0..cutoff] subtrees
        Glue.vbox ?? (rootView : subtreeViews)
    where
        makeItem idx val =
            Glue.hbox <*>
            ( [ [ Label.make "* " ]
                , [ makeInner val | idx < cutoff ]
                , [ Label.make "…" | idx == cutoff ]
                ] & concat & sequence
            )
            & Element.locallyAugmented (idx :: Int)
        cutoff = 4


makeRecord :: _ => Sugar.ResRecord Name (ResVal Name) -> M env m (WithTextPos View)
makeRecord (Sugar.ResRecord fields) =
    GridView.make <*> traverse (uncurry makeField) fields <&> snd
    <&> Align.WithTextPos 0

makeList :: _ => Sugar.ResList (ResVal Name) -> M env m (WithTextPos View)
makeList (Sugar.ResList head_) =
    do
        (preLabel, postLabel) <-
            Lens.view has <&>
            \case
            Dir.LeftToRight -> ("[", ", …]")
            Dir.RightToLeft -> ("]", "[… ,")
        c <- (TextView.make ?? postLabel) <*> (Element.subAnimId ?? ["]"]) <&> (^. Align.tValue)
        ((TextView.make ?? preLabel) <*> (Element.subAnimId ?? ["["])) /|/ makeInner head_
            >>= Align.tValue (hGlueAlign 1 ?? c)
    where
        hGlueAlign align l r =
            (pure (Aligned align l) /|/ pure (Aligned align r)) <&> (^. Align.value)

makeInject :: _ => Sugar.ResInject Name (ResVal Name) -> M env m (WithTextPos View)
makeInject (Sugar.ResInject tag mVal) =
    case mVal of
    Nothing -> TagView.make tag
    Just val -> TagView.make tag /|/ Spacer.stdHSpace /|/ makeInner val

depthCounts :: ResVal name -> [Int]
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
            (Draw.scaleV (image ^. Anim.iRect . Rect.size / view ^. Element.size) %%)

makeInner :: _ => ResVal Name -> M env m (WithTextPos View)
makeInner (Sugar.ResVal entityId body) =
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
    Sugar.RList x -> makeList x
    & advanceDepthParents
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId
        -- Only cut non-leaf expressions due to depth limits
        advanceDepthParents
            | Lens.has Lens.folded body = advanceDepth
            | otherwise = id

toText :: _ => r -> m (WithTextPos View)
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

make :: _ => ResVal Name -> m (WithTextPos View)
make v =
    do
        env <- Lens.view id
        let depthLimit =
                depthCounts v & scanl (+) 0 & tail
                & takeWhile (< env ^. has . Theme.maxEvalViewSize) & length
        runReaderT (makeInner v) Env{ _depthLeft = depthLimit, _base = env }
    <&> fixSize
