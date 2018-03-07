{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Transaction as Transaction
import qualified Data.Binary.Utils as BinUtils
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
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
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (EvalError(..), Val(..), Body(..))
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

data RecordStatus =
    RecordComputed | RecordExtendsError EvalError
    deriving (Eq, Ord, Show)

extractFields :: V.RecExtend (Val a) -> ([(T.Tag, Val a)], RecordStatus)
extractFields (V.RecExtend tag val (Val _ rest)) =
    case rest of
    RRecEmpty -> ([(tag, val)], RecordComputed)
    RRecExtend recExtend ->
        extractFields recExtend & _1 %~ ((tag, val):)
    RError err -> ([], RecordExtendsError err)
    x ->
        ( []
        , "extractFields expects record, got: " ++ show (void x)
          & EvalTypeError & RecordExtendsError
        )

textView ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> m (WithTextPos View)
textView text = (TextView.make ?? text) <*> Lens.view Element.animIdPrefix

label ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> m (WithTextPos View)
label text =
    textView text & Reader.local (Element.animIdPrefix <>~ [encodeUtf8 text])

makeTag :: Monad m => T.Tag -> ExprGuiM m (WithTextPos View)
makeTag tag =
    Anchors.assocTagNameRef tag & Transaction.getP
    <&> Lens.filtered Text.null .~ "(empty)"
    >>= textView

makeField :: Monad m => T.Tag -> Val Type -> ExprGuiM m [Aligned View]
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

makeError :: Monad m => EvalError -> ExprGuiM m (WithTextPos View)
makeError err =
    textView msg & Reader.local (Element.animIdPrefix <>~ ["error"])
    where
        msg =
            case err of
            EvalHole -> "?"
            _ -> Text.pack (show err)

arrayCutoff :: Int
arrayCutoff = 10

tableCutoff :: Int
tableCutoff = 6

makeArray :: Monad m => [Val Type] -> ExprGuiM m (WithTextPos View)
makeArray items =
    case sequence (items <&> (^? ER.body . ER._RRecExtend)) <&> Lens.mapped %~ extractFields of
    Just pairs@(x:_:_) | all (== RecordComputed) (pairs ^.. traverse . _2) ->
        do
            header <- mapM makeHeader tags
            rows <- pairs ^.. traverse . _1 & zip [0..tableCutoff-1] & mapM row
            s <- Spacer.stdHSpace
            let table =
                    header : rows <&> traverse %~ (^. Align.tValue)
                    <&> List.intersperse s
                    <&> traverse %~ Aligned 0.5
                    & GridView.make & Align.WithTextPos 0
            remainView <-
                if null (drop tableCutoff pairs)
                then pure Element.empty
                else label "…"
            Aligned 0.5 table /-/ Aligned 0.5 remainView ^. Align.value & pure
        where
            tags = x ^.. _1 . traverse . _1
            makeHeader tag =
                makeTag tag
                & Reader.local (Element.animIdPrefix <>~ [BinUtils.encodeS tag, "tag"])
            row (rowI, tagVals)
                | length tagVals /= length tags = error "makeArray: tags mismatch"
                | otherwise =
                    tags <&> (`lookup` tagVals) & sequence & fromMaybe (error "makeArray: tags mismatch")
                    & zip [(0::Int)..]
                    & traverse makeCell
                    & Reader.local (Element.animIdPrefix %~ (AnimId.augmentId ?? rowI))
                where
                    makeCell (colI, v) =
                        makeInner v
                        & Reader.local (Element.animIdPrefix %~ (AnimId.augmentId ?? colI))
    _ ->
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

makeRecExtend :: Monad m => Type -> V.RecExtend (Val Type) -> ExprGuiM m (WithTextPos View)
makeRecExtend typ recExtend =
    case
        ( typ, recStatus
        , lookup Builtins.rootTag fields, lookup Builtins.subtreesTag fields
        ) of
    (T.TInst tid _, RecordComputed, Just root, Just subtrees)
        | tid == Builtins.treeTid ->
        do
            rootView <-
                makeInner root & Reader.local (Element.animIdPrefix <>~ ["root"])
            subtreeViews <-
                subtrees ^.. ER.body . ER._RArray . Lens.traverse
                & zipWith makeItem [0..cutoff] & sequence
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
    _ ->
        do
            fieldsView <- mapM (uncurry makeField) fields <&> GridView.make
            let barWidth
                    | null fields = 150
                    | otherwise = fieldsView ^. Element.width
            restView <-
                case recStatus of
                RecordComputed -> pure Element.empty
                RecordExtendsError err ->
                    do
                        errView <- makeError err <&> (^. Align.tValue) <&> Aligned 0.5
                        sqr <-
                            View.unitSquare
                            & Reader.local (Element.animIdPrefix <>~ ["line"])
                            <&> Element.scale (Vector2 barWidth 1)
                            <&> Aligned 0.5
                        sqr /-/ errView & pure
            (Aligned 0.5 fieldsView /-/ restView) ^. Align.value
                & Align.WithTextPos 0 & pure
    where
        (fields, recStatus) = extractFields recExtend

makeInject :: Monad m => Type -> V.Inject (Val Type) -> ExprGuiM m (WithTextPos View)
makeInject typ inject =
    case
        ( typ
        , inject ^. V.injectVal . ER.body
        , mRecStatus
        , lookup Builtins.headTag fields
        , lookup Builtins.tailTag fields <&> (^. ER.body)
        ) of
    (_, RRecEmpty, _, _, _) -> makeTagView
    (T.TInst tid _, _, Just RecordComputed, Just head_, Just RFunc{})
        | tid == Builtins.streamTid ->
            do
                o <- label "["
                inner <- makeInner head_ & Reader.local (Element.animIdPrefix <>~ ["head"])
                c <- label ", …]" <&> (^. Align.tValue)
                o /|/ inner
                    & Align.tValue %~ (hGlueAlign 1 ?? c)
                    & pure
    _ ->
        do
            tag <- makeTagView
            s <- Spacer.stdHSpace
            i <-
                inject ^. V.injectVal & makeInner
                & Reader.local (Element.animIdPrefix <>~ ["val"])
            tag /|/ s /|/ i & pure
    where
        hGlueAlign align l r = (Aligned align l /|/ Aligned align r) ^. Align.value
        makeTagView =
            inject ^. V.injectTag & makeTag
            & Reader.local (Element.animIdPrefix <>~ ["tag"])
        (fields, mRecStatus) =
            case inject ^. V.injectVal . ER.body of
            RRecExtend recExtend -> extractFields recExtend & _2 %~ Just
            _ -> ([], Nothing)

depthCounts :: Val a -> [Int]
depthCounts v =
    v ^.. ER.body . Lens.folded
    & take arrayCutoff
    <&> depthCounts
    & List.transpose
    <&> sum
    & (1 :)

make :: Monad m => Val Type -> ExprGuiM m (WithTextPos View)
make v =
    do
        maxEvalViewSize <- Lens.view Theme.theme <&> Theme.maxEvalViewSize
        let depthLimit =
                depthCounts v & scanl (+) 0 & tail
                & takeWhile (< maxEvalViewSize) & length
        makeInner v
            & ExprGuiM.resetDepth depthLimit
    <&> fixSize

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

makeInner :: Monad m => Val Type -> ExprGuiM m (WithTextPos View)
makeInner (Val typ val) =
    do
        animId <- Lens.view Element.animIdPrefix
        case val of
            RError err -> makeError err
            RFunc{} -> textView "Fn"
            RRecEmpty -> textView "()"
            RInject inject -> makeInject typ inject
            RRecExtend recExtend -> makeRecExtend typ recExtend
            RPrimVal primVal
                | typ == T.TInst Builtins.textTid mempty ->
                  case pv of
                  PrimVal.Bytes x ->
                    case decodeUtf8' x of
                    Right txt -> toText txt
                    Left{} -> toText x
                  _ -> makeError (EvalTypeError "text not made of bytes")
                | otherwise ->
                  case pv of
                  PrimVal.Bytes x -> toText x
                  PrimVal.Float x -> toText x
                where
                    pv = PrimVal.toKnown primVal
            RArray items -> makeArray items
            & advanceDepth animId
    where
        -- Only cut non-leaf expressions due to depth limits
        advanceDepth animId
            | Lens.has traverse val = ExprGuiM.advanceDepth pure animId
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
