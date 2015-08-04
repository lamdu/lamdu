{-# LANGUAGE OverloadedStrings #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.Binary.Utils as BinUtils
import qualified Data.Store.Transaction as Transaction
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Eval.Val (EvalResult, Val(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

data RecordStatus = RecordComputed | RecordNotFinished

extractFields ::
    V.RecExtend (EvalResult ()) -> ([(T.Tag, EvalResult ())], RecordStatus)
extractFields (V.RecExtend tag val rest) =
    case rest of
    HRecEmpty -> ([(tag, val)], RecordComputed)
    HRecExtend recExtend ->
        extractFields recExtend & _1 %~ ((tag, val):)
    _ -> error "RecExtend of non-record"

textView :: MonadA m => String -> AnimId -> ExprGuiM m View
textView x animId = BWidgets.makeTextView x animId & ExprGuiM.widgetEnv

makeTag :: MonadA m => AnimId -> T.Tag -> ExprGuiM m View
makeTag animId tag =
    Anchors.assocNameRef tag & Transaction.getP & ExprGuiM.transaction
    >>= (`textView` animId)

makeField ::
    MonadA m =>
    AnimId -> T.Tag -> EvalResult () -> ExprGuiM m [(GridView.Alignment, View)]
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

make :: MonadA m => AnimId -> EvalResult () -> ExprGuiM m View
make animId val =
    case val of
    HFunc{} -> textView "Fn" animId
    HAbsurd -> textView "Fn" animId
    HCase{} -> textView "Fn" animId
    HRecEmpty -> textView "Ø" animId
    HInject (V.Inject injTag HRecEmpty) -> makeTag (animId ++ ["tag"]) injTag
    HInject inj ->
        do
            tagView <- inj ^. V.injectTag & makeTag (animId ++ ["tag"])
            space <-
                ExprGuiM.readConfig
                <&> Spacer.makeHorizontal . realToFrac . Config.spaceWidth
            valView <- inj ^. V.injectVal & make (animId ++ ["val"])
            GridView.horizontalAlign 0.5 [tagView, space, valView] & return
    HRecExtend recExtend ->
        do
            fieldsView <- mapM (uncurry (makeField animId)) fields <&> GridView.make
            let barWidth
                    | null fields = 150
                    | otherwise = fieldsView ^. View.width
            restView <-
                case recStatus of
                RecordComputed -> return View.empty
                RecordNotFinished ->
                    do
                        let sqr =
                                View 1 (Anim.unitSquare (animId ++ ["line"]))
                                & View.scale (Vector2 barWidth 1)
                        v <- textView "?" (animId ++ ["?"])
                        return $ GridView.verticalAlign 0.5 [sqr, v]
            GridView.verticalAlign 0.5 [fieldsView, restView] & return
        where
            (fields, recStatus) = extractFields recExtend
    body ->
        BWidgets.makeTextView text animId & ExprGuiM.widgetEnv
        where
            text = show body & truncateStr 20
    & ExprGuiM.advanceDepth return animId

truncateStr :: Int -> String -> String
truncateStr n s
    | l > n = take (n `div` 3) s ++ ".." ++ drop (l - (2 * n `div` 3)) s
    | otherwise = s
    where
        l = length s
