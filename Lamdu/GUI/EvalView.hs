{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Binary.Utils as BinUtils
import qualified Data.List as List
import qualified Data.Store.Transaction as Transaction
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (EvalError(..), Val(..), Body(..))
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

data RecordStatus = RecordComputed | RecordExtendsError EvalError

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

textView :: Monad m => Text -> AnimId -> ExprGuiM m View
textView x animId = BWidgets.makeTextView x animId & ExprGuiM.widgetEnv

label :: Monad m => Text -> AnimId -> ExprGuiM m View
label x animId = BWidgets.makeTextView x (Anim.augmentId animId x) & ExprGuiM.widgetEnv

makeTag :: Monad m => AnimId -> T.Tag -> ExprGuiM m View
makeTag animId tag =
    Anchors.assocNameRef tag & Transaction.getP & ExprGuiM.transaction
    >>= (`textView` animId)

makeField ::
    Monad m =>
    AnimId -> T.Tag -> Val Type -> ExprGuiM m [(GridView.Alignment, View)]
makeField parentAnimId tag val =
    do
        tagView <- makeTag (baseId ++ ["tag"]) tag
        space <- ExprGuiM.widgetEnv BWidgets.stdHSpaceView
        valView <- makeInner (baseId ++ ["val"]) val
        return
            [ (GridView.Alignment (Vector2 1 0.5), tagView)
            , (0.5, space)
            , (GridView.Alignment (Vector2 0 0.5), valView)
            ]
    where
        baseId = parentAnimId ++ [BinUtils.encodeS tag]

makeError :: Monad m => EvalError -> AnimId -> ExprGuiM m View
makeError err animId =
    textView msg $ animId ++ ["error"]
    where
        msg =
            case err of
            EvalHole -> "?"
            _ -> Text.pack (show err)

hbox :: [View] -> View
hbox = GridView.horizontalAlign 0.5

arrayCutoff :: Int
arrayCutoff = 10

makeArray :: Monad m => AnimId -> [Val Type] -> ExprGuiM m View
makeArray animId items =
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
    Monad m => AnimId -> Type -> V.RecExtend (Val Type) -> ExprGuiM m View
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
            rootView : subtreeViews & GridView.verticalAlign 0 & return
        where
            makeItem idx val =
                [ [ label "* " itemId ]
                , [ makeInner (Anim.augmentId itemId ("val" :: Text)) val
                    | idx < cutoff ]
                , [ label "..." itemId | idx == cutoff ]
                ] & concat
                & sequence
                <&> GridView.horizontalAlign 0
                where
                    itemId = Anim.augmentId animId (idx :: Int)
            cutoff = 4
    _ ->
        do
            fieldsView <- mapM (uncurry (makeField animId)) fields <&> GridView.make
            let barWidth
                    | null fields = 150
                    | otherwise = fieldsView ^. View.width
            restView <-
                case recStatus of
                RecordComputed -> return View.empty
                RecordExtendsError err ->
                    do
                        v <- makeError err animId
                        GridView.verticalAlign 0.5 [sqr, v] & return
                    where
                        sqr =
                            View.make 1 (Anim.unitSquare (animId ++ ["line"]))
                            & View.scale (Vector2 barWidth 1)
            GridView.verticalAlign 0.5 [fieldsView, restView] & return
    where
        (fields, recStatus) = extractFields recExtend

depthCounts :: Val a -> [Int]
depthCounts v =
    v ^.. ER.body . Lens.folded
    & take arrayCutoff
    <&> depthCounts
    & List.transpose
    <&> sum
    & (1 :)

make :: Monad m => AnimId -> Val Type -> ExprGuiM m View
make animId v =
    do
        maxEvalViewSize <- ExprGuiM.readConfig <&> Config.maxEvalViewSize
        let depthLimit =
                depthCounts v & scanl (+) 0 & tail
                & takeWhile (< maxEvalViewSize) & length
        makeInner animId v
            & ExprGuiM.resetDepth depthLimit

makeInner :: Monad m => AnimId -> Val Type -> ExprGuiM m View
makeInner animId (Val typ val) =
    case val of
    RError err -> makeError err animId
    RFunc{} -> textView "Fn" animId
    RRecEmpty -> textView "()" animId
    RInject (V.Inject injTag (Val _ RRecEmpty)) ->
        makeTag (animId ++ ["tag"]) injTag
    RInject inj ->
        do
            tagView <- inj ^. V.injectTag & makeTag (animId ++ ["tag"])
            space <- ExprGuiM.widgetEnv BWidgets.stdHSpaceView
            valView <- inj ^. V.injectVal & makeInner (animId ++ ["val"])
            hbox [tagView, space, valView] & return
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
            toText :: (Format r, Monad m) => r -> ExprGuiM m View
            toText = asText . format
    RArray items -> makeArray animId items
    & ExprGuiM.advanceDepth return animId
    where
        asText text =
            BWidgets.makeTextView cut animId & ExprGuiM.widgetEnv
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
