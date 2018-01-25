{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveTraversable, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.IfElseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Functor.Compose (Compose(..))
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import           Lamdu.GUI.ExpressionEdit.BinderEdit (addLetEventMap)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data Row a = Row
    { _rIndentId :: AnimId
    , _rKeyword :: a
    , _rPredicate :: a
    , _rResult :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Row

makeIfThen ::
    ( Monad m, MonadReader env f, HasTheme env, HasConfig env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    ) =>
    WithTextPos View -> Sugar.EntityId -> Sugar.IfThen (T m) (ExpressionGui m) ->
    f (Row (ExpressionGui m))
makeIfThen prefixLabel entityId ifThen =
    do
        label <- Styled.grammarLabel "if "
        colon <- Styled.grammarLabel ": "
        let keyword = prefixLabel /|/ label & Responsive.fromTextView
        config <- Lens.view Config.config
        let eventMap =
                ifThen ^. Sugar.itDelete <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Delete"])
        Row indentAnimId keyword
            (Widget.weakerEvents eventMap ((ifThen ^. Sugar.itIf) /|/ colon))
            (Widget.weakerEvents eventMap (ifThen ^. Sugar.itThen))
            & pure
    where
        indentAnimId = WidgetIds.fromEntityId entityId & Widget.toAnimId

makeElseIf ::
    Monad m =>
    Sugar.ElseIf (T m) (ExprGui.SugarExpr m) ->
    ExprGuiM m [Row (ExpressionGui m)] -> ExprGuiM m [Row (ExpressionGui m)]
makeElseIf (Sugar.ElseIf scopes entityId ifThen addLet) makeRest =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        let mInnerScope = lookupMKey <$> mOuterScopeId <*> scopes
        -- TODO: green evaluation backgrounds, "◗"?
        elseLabel <- Styled.grammarLabel "el"
        letEventMap <- addLetEventMap addLet
        (:)
            <$>
            ( traverse ExprGuiM.makeSubexpression ifThen
                <&> Sugar.itIf %~ Widget.weakerEvents letEventMap
                >>= makeIfThen elseLabel entityId
            )
            <*>  makeRest
            & Reader.local (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromEntityId entityId))
            & ExprGuiM.withLocalMScopeId mInnerScope
    where
        -- TODO: cleaner way to write this?
        lookupMKey k m = k >>= (`Map.lookup` m)

makeElse :: Monad m => ExprGui.SugarExpr m -> ExprGuiM m (Row (ExpressionGui m))
makeElse expr =
    ( Row elseAnimId
        <$> (Styled.grammarLabel "else" <&> Responsive.fromTextView)
        <*> (Styled.grammarLabel ": " & Reader.local (Element.animIdPrefix .~ elseAnimId) <&> Responsive.fromTextView)
    ) <*> ExprGuiM.makeSubexpression expr
    where
        elseAnimId = Widget.toAnimId elseId
        elseId = WidgetIds.fromExprPayload (expr ^. Sugar.rPayload)

verticalRowRender ::
    ( Monad m, MonadReader env f, Spacer.HasStdSpacing env
    , ResponsiveExpr.HasStyle env
    ) => f (Row (ExpressionGui m) -> ExpressionGui m)
verticalRowRender =
    do
        indent <- ResponsiveExpr.indent
        vbox <- Responsive.vboxSpaced
        return $
            \row ->
            vbox
            [ Options.box Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
            , indent (row ^. rIndentId) (row ^. rResult)
            ]

renderRows ::
    ( Monad m, MonadReader env f, Spacer.HasStdSpacing env
    , ResponsiveExpr.HasStyle env
    ) => f ([Row (ExpressionGui m)] -> ExpressionGui m)
renderRows =
    do
        vspace <- Spacer.getSpaceSize <&> (^._2)
        -- TODO: better way to make space between rows in grid??
        let spaceAbove = (<&> Element.assymetricPad (Vector2 0 vspace) 0)
        let prepareRows [] = []
            prepareRows [x, y] = [prep2 x, spaceAbove (prep2 y)]
            prepareRows (x:xs) = x : (xs <&> spaceAbove)
        vert <- verticalRowRender
        vbox <- Responsive.vboxSpaced
        return $
            \rows ->
            vbox (rows <&> vert)
            & Options.tryWideLayout Options.table (Compose (prepareRows rows))
    where
        -- When there's only "if" and "else", we want to merge the predicate with the keyword
        -- because there are no several predicates to be aligned
        prep2 row =
            row
            & rKeyword .~ Options.box Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
            & rPredicate .~ Element.empty

make ::
    Monad m =>
    Sugar.IfElse (T m) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make ifElse pl =
    stdWrapParentExpr pl
    <*> ( renderRows
            <*>
            ( (:)
                <$> makeIf
                <*> foldr makeElseIf (makeElse (ifElse ^. Sugar.iElse) <&> (:[])) (ifElse ^. Sugar.iElseIfs)
            )
        )
    where
        makeIf =
            ifElse ^. Sugar.iIfThen
            & traverse ExprGuiM.makeSubexpression
            >>= makeIfThen Element.empty (pl ^. Sugar.plEntityId)
