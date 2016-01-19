{-# LANGUAGE OverloadedStrings #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Control.MonadA (MonadA)
import           Data.Binary.Utils (decodeS)
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
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Results (Val(..), Body(..))
import           Lamdu.Eval.Val (EvalError(..))
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

textView :: MonadA m => String -> AnimId -> ExprGuiM m View
textView x animId = BWidgets.makeTextView x animId & ExprGuiM.widgetEnv

makeTag :: MonadA m => AnimId -> T.Tag -> ExprGuiM m View
makeTag animId tag =
    Anchors.assocNameRef tag & Transaction.getP & ExprGuiM.transaction
    >>= (`textView` animId)

makeField ::
    MonadA m =>
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

makeError :: MonadA m => EvalError -> AnimId -> ExprGuiM m View
makeError err animId = textView msg $ animId ++ ["error"]
    where
        msg =
            case err of
            EvalHole -> "?"
            _ -> show err

make :: MonadA m => AnimId -> Val Type -> ExprGuiM m View
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
            GridView.horizontalAlign 0.5 [tagView, space, valView] & return
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
    RLiteral l@(V.Literal tId x)
        | typ == T.TInst Builtins.textTid mempty ->
            if tId == Builtins.bytesId
            then (asText . format . UTF8.toString) x
            else error "text not made of bytes"
        | tId == Builtins.floatId -> (asText . (format :: Double -> String) . decodeS) x
        | tId == Builtins.bytesId -> (asText . format) x
        | otherwise -> asText (show l)
    & ExprGuiM.advanceDepth return animId
    where
        asText text = BWidgets.makeTextView text animId & ExprGuiM.widgetEnv
