{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeCommon ::
    Monad m =>
    Sugar.Tag (Name m) m ->
    NearestHoles -> [ExpressionGui m] ->
    ExprGuiM m (ExpressionGui m)
makeCommon tag nearestHoles valEdits =
    (Options.boxSpaced ?? Options.disambiguationNone)
    <*> ( TagEdit.makeCaseTag nearestHoles tag
          <&> Responsive.fromWithTextPos <&> (: valEdits)
        )

make ::
    Monad m =>
    Sugar.Inject (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tag mVal) pl =
    case mVal of
    Nothing ->
        makeCommon
        -- Give the tag widget the identity of the whole inject
        (tag & Sugar.tagInfo . Sugar.tagInstance .~ (pl ^. Sugar.plEntityId))
        (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) []
        & ExpressionGui.stdWrap pl
    Just val ->
        ExprGuiM.makeSubexpressionWith ApplyEdit.prefixPrecedence
        (ExpressionGui.before .~ ApplyEdit.prefixPrecedence) val <&> (:[])
        >>= makeCommon tag (ExprGuiT.nextHolesBefore val)
        & ExpressionGui.stdWrapParentExpr pl (tag ^. Sugar.tagInfo . Sugar.tagInstance)
