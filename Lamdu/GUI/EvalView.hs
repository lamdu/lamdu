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
import           Lamdu.Eval.Val (EvalResult, Val(..), EvalError(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

data RecordStatus = RecordComputed | RecordExtendsError EvalError

extractFields ::
    V.RecExtend (EvalResult ()) -> ([(T.Tag, EvalResult ())], RecordStatus)
extractFields (V.RecExtend tag val rest) =
    case rest of
    Right HRecEmpty -> ([(tag, val)], RecordComputed)
    Right (HRecExtend recExtend) ->
        extractFields recExtend & _1 %~ ((tag, val):)
    Right x ->
        ( []
        , "extractFields expects record, got: " ++ show x
          & EvalTypeError & RecordExtendsError
        )
    Left err -> ([], RecordExtendsError err)

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

makeError :: MonadA m => AnimId -> EvalError -> ExprGuiM m View
makeError animId err = textView msg $ animId ++ ["error"]
    where
        msg =
            case err of
            EvalHole -> "?"
            _ -> show err

make :: MonadA m => AnimId -> EvalResult () -> ExprGuiM m View
make animId (Left err) = makeError animId err
make animId (Right val) = makeForVal animId val

makeForVal :: MonadA m => AnimId -> Val () -> ExprGuiM m View
makeForVal animId val =
    case val of
    HFunc{} -> textView "Fn" animId
    HAbsurd -> textView "Fn" animId
    HCase{} -> textView "Fn" animId
    HRecEmpty -> textView "Ø" animId
    HInject (V.Inject injTag (Right HRecEmpty)) ->
        makeTag (animId ++ ["tag"]) injTag
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
                RecordExtendsError err ->
                    do
                        v <- makeError animId err
                        GridView.verticalAlign 0.5 [sqr, v] & return
                    where
                        sqr =
                            View 1 (Anim.unitSquare (animId ++ ["line"]))
                            & View.scale (Vector2 barWidth 1)
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
