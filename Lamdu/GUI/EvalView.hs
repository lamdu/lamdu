{-# LANGUAGE OverloadedStrings #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)

import qualified Data.Binary.Utils as BinUtils
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Store.Transaction as Transaction
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (EvalError(..), Val(..), Body(..))
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

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

textView :: Monad m => String -> AnimId -> ExprGuiM m View
textView x animId = BWidgets.makeTextView x animId & ExprGuiM.widgetEnv

label :: Monad m => String -> AnimId -> ExprGuiM m View
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
        space <-
            ExprGuiM.readConfig
            <&> Spacer.makeHorizontal . realToFrac . Config.spaceWidth
        valView <- make (baseId ++ ["val"]) val
        return
            [ (Vector2 1 0.5, tagView)
            , (0.5, space)
            , (Vector2 0 0.5, valView)
            ]
    where
        baseId = parentAnimId ++ [BinUtils.encodeS tag]

makeError :: Monad m => EvalError -> AnimId -> ExprGuiM m View
makeError err animId = textView msg $ animId ++ ["error"]
    where
        msg =
            case err of
            EvalHole -> "?"
            _ -> show err

hbox :: [View] -> View
hbox = GridView.horizontalAlign 0.5

makeArray :: Monad m => AnimId -> [Val Type] -> ExprGuiM m View
makeArray animId items =
    do
        itemViews <- zipWith makeItem [0..cutoff] items & sequence
        opener <- label "[" animId
        closer <- label "]" animId
        opener : itemViews ++ [closer] & hbox & return
    where
        cutoff = 9
        makeItem idx val =
            [ [ label ", " itemId | idx > 0 ]
            , [ make (Anim.augmentId itemId ("val" :: String)) val ]
            , [ label "..." itemId | idx == cutoff ]
            ] & concat
            & sequence
            <&> hbox
            where
                itemId = Anim.augmentId animId (idx :: Int)

make :: Monad m => AnimId -> Val Type -> ExprGuiM m View
make animId (Val typ val) =
    case val of
    RError err -> makeError err animId
    RFunc{} -> textView "Fn" animId
    RRecEmpty -> textView "()" animId
    RInject (V.Inject injTag (Val _ RRecEmpty)) ->
        makeTag (animId ++ ["tag"]) injTag
    RInject inj ->
        do
            tagView <- inj ^. V.injectTag & makeTag (animId ++ ["tag"])
            space <-
                ExprGuiM.readConfig
                <&> Spacer.makeHorizontal . realToFrac . Config.spaceWidth
            valView <- inj ^. V.injectVal & make (animId ++ ["val"])
            hbox [tagView, space, valView] & return
    RRecExtend recExtend ->
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
                            View 1 (Anim.unitSquare (animId ++ ["line"]))
                            & View.scale (Vector2 barWidth 1)
            GridView.verticalAlign 0.5 [fieldsView, restView] & return
        where
            (fields, recStatus) = extractFields recExtend
    RPrimVal primVal
        | typ == T.TInst Builtins.textTid mempty ->
          case pv of
          PrimVal.Bytes x -> UTF8.toString x & toText
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
        asText text = BWidgets.makeTextView text animId & ExprGuiM.widgetEnv
