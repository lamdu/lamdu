{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Transaction as Transaction
import qualified Data.Binary.Utils as BinUtils
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
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

-- TODO: Remove
textView :: Monad m => Text -> AnimId -> ExprGuiM m (WithTextPos View)
textView x animId = TextView.make ?? x ?? animId

label :: Monad m => Text -> AnimId -> ExprGuiM m (WithTextPos View)
label x animId =
    TextView.make ?? x ?? Anim.augmentId animId x

makeTag :: Monad m => AnimId -> T.Tag -> ExprGuiM m (WithTextPos View)
makeTag animId tag =
    Anchors.assocNameRef tag & Transaction.getP
    >>= (`textView` animId)

makeField ::
    Monad m =>
    AnimId -> T.Tag -> Val Type -> ExprGuiM m [Aligned View]
makeField parentAnimId tag val =
    do
        tagView <- makeTag (baseId ++ ["tag"]) tag
        space <- Spacer.stdHSpace
        valView <- makeInner (baseId ++ ["val"]) val
        return
            [ toAligned 1 tagView
            , Aligned 0.5 space
            , toAligned 0 valView
            ]
    where
        toAligned x (WithTextPos y w) =
            Aligned (Vector2 x (y / w ^. Element.height)) w
        baseId = parentAnimId ++ [BinUtils.encodeS tag]

makeError :: Monad m => EvalError -> AnimId -> ExprGuiM m (WithTextPos View)
makeError err animId =
    textView msg $ animId ++ ["error"]
    where
        msg =
            case err of
            EvalHole -> "?"
            _ -> Text.pack (show err)

arrayCutoff :: Int
arrayCutoff = 10

tableCutoff :: Int
tableCutoff = 6

makeArray :: Monad m => AnimId -> [Val Type] -> ExprGuiM m (WithTextPos View)
makeArray animId items =
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
                then return Element.empty
                else label "..." animId
            Aligned 0.5 table /-/ Aligned 0.5 remainView ^. Align.value & return
        where
            tags = x ^.. _1 . traverse . _1
            makeHeader tag = makeTag (AnimId.augmentId animId tag) tag
            row (rowI, tagVals)
                | length tagVals /= length tags = error "makeArray: tags mismatch"
                | otherwise =
                    tags <&> (`lookup` tagVals) & sequence & fromMaybe (error "makeArray: tags mismatch")
                    & zip [(0::Int)..]
                    & mapM makeCell
                where
                    rowId = AnimId.augmentId animId rowI
                    makeCell (colI, v) = makeInner (AnimId.augmentId rowId colI) v
    _ ->
        do
            itemViews <- zipWith makeItem [0..arrayCutoff] items & sequence
            opener <- label "[" animId
            closer <- label "]" animId
            opener : itemViews ++ [closer] & hbox & return
    where
        makeItem idx val =
            [ [ label ", " itemId | idx > 0 ]
            , [ makeInner (Anim.augmentId itemId ("val" :: Text)) val
                | idx < arrayCutoff ]
            , [ label "..." itemId | idx == arrayCutoff ]
            ] & concat
            & sequence
            <&> hbox
            where
                itemId = Anim.augmentId animId (idx :: Int)

makeRecExtend ::
    Monad m =>
    AnimId -> Type -> V.RecExtend (Val Type) -> ExprGuiM m (WithTextPos View)
makeRecExtend animId typ recExtend =
    case
        ( typ, recStatus
        , lookup Builtins.rootTag fields, lookup Builtins.subtreesTag fields
        ) of
    (T.TInst tid _, RecordComputed, Just root, Just subtrees)
        | tid == Builtins.treeTid ->
        do
            rootView <- makeInner (animId ++ ["root"]) root
            subtreeViews <-
                subtrees ^.. ER.body . ER._RArray . Lens.traverse
                & zipWith makeItem [0..cutoff] & sequence
            rootView : subtreeViews & vbox & return
        where
            makeItem idx val =
                [ [ label "* " itemId ]
                , [ makeInner (Anim.augmentId itemId ("val" :: Text)) val
                    | idx < cutoff ]
                , [ label "..." itemId | idx == cutoff ]
                ] & concat
                & sequence
                <&> hbox
                where
                    itemId = Anim.augmentId animId (idx :: Int)
            cutoff = 4
    _ ->
        do
            fieldsView <- mapM (uncurry (makeField animId)) fields <&> GridView.make
            let barWidth
                    | null fields = 150
                    | otherwise = fieldsView ^. Element.width
            restView <-
                case recStatus of
                RecordComputed -> return Element.empty
                RecordExtendsError err ->
                    makeError err animId
                    <&> (^. Align.tValue)
                    <&> Aligned 0.5
                    <&> (sqr /-/)
                    where
                        sqr =
                            View.unitSquare (animId ++ ["line"])
                            & Element.scale (Vector2 barWidth 1)
                            & Aligned 0.5
            (Aligned 0.5 fieldsView /-/ restView) ^. Align.value & Align.WithTextPos 0 & return
    where
        (fields, recStatus) = extractFields recExtend

makeInject ::
    Monad m => AnimId -> Type -> V.Inject (Val Type) -> ExprGuiM m (WithTextPos View)
makeInject animId typ inject =
    case
        ( typ
        , inject ^. V.injectVal . ER.body
        , mRecStatus
        , lookup Builtins.headTag fields
        , lookup Builtins.tailTag fields <&> (^. ER.body)
        ) of
    (_, RRecEmpty, _, _, _) -> makeTagView
    (T.TInst tid _, _, Just RecordComputed, Just head_, Just RFunc)
        | tid == Builtins.streamTid ->
        [ label "[" animId
        , makeInner (animId ++ ["head"]) head_
        , label ", …]" animId
        ]
        & sequence <&> hbox
    _ ->
        do
            tag <- makeTagView
            s <- Spacer.stdHSpace
            i <- inject ^. V.injectVal & makeInner (animId ++ ["val"])
            tag /|/ s /|/ i & pure
    where
        makeTagView = inject ^. V.injectTag & makeTag (animId ++ ["tag"])
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

make :: Monad m => AnimId -> Val Type -> ExprGuiM m (WithTextPos View)
make animId v =
    do
        maxEvalViewSize <- Lens.view Theme.theme <&> Theme.maxEvalViewSize
        let depthLimit =
                depthCounts v & scanl (+) 0 & tail
                & takeWhile (< maxEvalViewSize) & length
        makeInner animId v
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

makeInner :: Monad m => AnimId -> Val Type -> ExprGuiM m (WithTextPos View)
makeInner animId (Val typ val) =
    case val of
    RError err -> makeError err animId
    RFunc{} -> textView "Fn" animId
    RRecEmpty -> textView "()" animId
    RInject inject -> makeInject animId typ inject
    RRecExtend recExtend -> makeRecExtend animId typ recExtend
    RPrimVal primVal
        | typ == T.TInst Builtins.textTid mempty ->
          case pv of
          PrimVal.Bytes x -> decodeUtf8 x & toText
          _ -> error "text not made of bytes"
        | otherwise ->
          case pv of
          PrimVal.Bytes x -> toText x
          PrimVal.Float x -> toText x
        where
            pv = PrimVal.toKnown primVal
            toText :: (Format r, Monad m) => r -> ExprGuiM m (WithTextPos View)
            toText = asText . format
    RArray items -> makeArray animId items
    & advanceDepth
    where
        -- Only cut non-leaf expressions due to depth limits
        advanceDepth
            | Lens.has traverse val = ExprGuiM.advanceDepth return animId
            | otherwise = id
        asText text =
            TextView.make ?? cut ?? animId
            where
                cut =
                    map limLine start ++
                    ( case rest of
                      [] -> []
                      _ -> ["..."]
                    ) & Text.intercalate "\n"
                    where
                        (start, rest) = splitAt 10 (Text.lines text)
                limLine :: Text -> Text
                limLine ln =
                    start <>
                    if Text.null rest
                    then ""
                    else "..."
                    where
                        (start, rest) = Text.splitAt 100 ln
