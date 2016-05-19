{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.List as List
import qualified Graphics.UI.Bottle.Font as Font
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.CaseEdit as CaseEdit
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.InjectEdit as InjectEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.NomEdit as NomEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGuiM(..), ExpressionGui)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

shrinkIfHigherThanLine :: Monad m => ExprGuiM m (ExpressionGui f -> ExpressionGui f)
shrinkIfHigherThanLine =
    do
        sizedFont <-
            ExprGuiM.widgetEnv WE.readTextStyle
            <&> (^. TextEdit.sTextViewStyle . TextView.styleFont)
        config <- ExprGuiM.readConfig <&> Config.hole
        return
            $ \(ExpressionGui mkLayout) ->
            ExpressionGui $
            \layoutMode ->
            let layout = mkLayout layoutMode
                ratio =
                    (Font.height sizedFont /
                     layout ^. Layout.widget . Widget.height)
                    ** realToFrac (Config.holeResultInjectedScaleExponent config)
                result
                    | ratio >= 1 = layout
                    | otherwise =
                          layout
                          & Layout.scale (realToFrac ratio)
                          & Layout.alignment . _2 .~ 0.5
            in result

make :: Monad m => ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make sExpr =
    maybeShrink <*> makeEditor body pl
    & assignCursor
    where
        Sugar.Expression body pl = sExpr
        exprHiddenEntityIds =
            List.delete (pl ^. Sugar.plEntityId)
            (pl ^. Sugar.plData ^. ExprGuiT.plStoredEntityIds)
        myId = WidgetIds.fromExprPayload pl
        maybeShrink
            | or (pl ^. Sugar.plData ^. ExprGuiT.plInjected) = shrinkIfHigherThanLine
            | otherwise = pure id
        assignCursor x =
            foldr (`ExprGuiM.assignCursorPrefix` const myId) x $
            exprHiddenEntityIds <&> WidgetIds.fromEntityId

makeEditor ::
    Monad m =>
    Sugar.Body (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeEditor body =
    case body of
    Sugar.BodyHole         x -> x & HoleEdit.make
    Sugar.BodyApply        x -> x & ApplyEdit.make
    Sugar.BodyLam          x -> x & LambdaEdit.make
    Sugar.BodyLiteral      x -> x & LiteralEdit.make
    Sugar.BodyRecord       x -> x & RecordEdit.make
    Sugar.BodyCase         x -> x & CaseEdit.make
    Sugar.BodyGetField     x -> x & GetFieldEdit.make
    Sugar.BodyInject       x -> x & InjectEdit.make
    Sugar.BodyGetVar       x -> x & GetVarEdit.make
    Sugar.BodyToNom        x -> x & NomEdit.makeToNom
    Sugar.BodyFromNom      x -> x & NomEdit.makeFromNom
