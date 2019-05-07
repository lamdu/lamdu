{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.IfElseEdit
    ( make
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Functor.Compose (Compose(..))
import qualified Data.Map as Map
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Row a = Row
    { _rIndentId :: AnimId
    , _rKeyword :: a
    , _rPredicate :: a
    , _rResult :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Row

makeIfThen ::
    (Monad i, Monad o) =>
    WithTextPos View -> AnimId ->
    Tree (Sugar.IfElse (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    ExprGuiM i o (Row (Gui Responsive o))
makeIfThen prefixLabel animId ifElse =
    do
        ifGui <-
            ExprGuiM.makeSubexpression (ifElse ^. Sugar.iIf)
            /|/ grammar (label (Texts.code . Texts.condColon))
        thenGui <- ExprGuiM.makeSubexpression (ifElse ^. Sugar.iThen)
        keyword <-
            pure prefixLabel
            /|/ grammar (label (Texts.code . Texts.if_))
            <&> Responsive.fromTextView
        config <- Lens.view Config.config
        let eventMap =
                foldMap
                (E.keysEventMapMovesCursor (Config.delKeys config)
                 (E.Doc ["Edit", "Delete"]) . fmap WidgetIds.fromEntityId)
                (ifElse ^. Sugar.iElse . ann .
                 Sugar.plActions . Sugar.mReplaceParent)
        Row animId keyword
            (Widget.weakerEvents eventMap ifGui)
            (Widget.weakerEvents eventMap thenGui)
            & pure

makeElseBody ::
    (Monad i, Monad o) =>
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    Tree (Sugar.Else (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    ExprGuiM i o [Row (Gui Responsive o)]
makeElseBody pl (Sugar.SimpleElse expr) =
    ( Row elseAnimId
        <$> (grammar (label (Texts.code . Texts.else_)) <&> Responsive.fromTextView)
        <*> (grammar (label (Texts.code . Texts.condColon))
                & Reader.local (Element.animIdPrefix .~ elseAnimId)
                <&> Responsive.fromTextView)
    ) <*> ExprGuiM.makeSubexpression (Ann pl expr)
    <&> pure
    where
        elseAnimId = WidgetIds.fromExprPayload pl & Widget.toAnimId
makeElseBody pl (Sugar.ElseIf (Sugar.ElseIfContent scopes content)) =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        let mInnerScope = lookupMKey <$> mOuterScopeId <*> scopes
        -- TODO: green evaluation backgrounds, "◗"?
        elseLabel <- grammar (label (Texts.code . Texts.elseShort))
        letEventMap <-
            foldMap ExprEventMap.addLetEventMap (pl ^. Sugar.plActions . Sugar.mNewLet)
        (:)
            <$> ( makeIfThen elseLabel animId content
                  <&> Lens.mapped %~ Widget.weakerEvents letEventMap
                )
            <*> makeElse (content ^. Sugar.iElse)
            & Reader.local (Element.animIdPrefix .~ animId)
            & ExprGuiM.withLocalMScopeId mInnerScope
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId
        entityId = pl ^. Sugar.plEntityId
        -- TODO: cleaner way to write this?
        lookupMKey k m = k >>= (`Map.lookup` m)

-- TODO inline and use "case"
makeElse ::
    (Monad i, Monad o) =>
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) (Sugar.Else (Name o) i o) ->
    ExprGuiM i o [Row (Gui Responsive o)]
makeElse (Ann pl x) = makeElseBody pl x

verticalRowRender ::
    ( Monad o, MonadReader env f, Spacer.HasStdSpacing env
    , ResponsiveExpr.HasStyle env, Glue.HasTexts env
    ) => f (Row (Gui Responsive o) -> Gui Responsive o)
verticalRowRender =
    do
        indent <- ResponsiveExpr.indent
        obox <- Options.box
        vbox <- Responsive.vboxSpaced
        pure $
            \row ->
            vbox
            [ obox Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
            , indent (row ^. rIndentId) (row ^. rResult)
            ]

renderRows ::
    ( Monad o, MonadReader env f, Spacer.HasStdSpacing env
    , ResponsiveExpr.HasStyle env
    , Grid.HasTexts env
    ) => Maybe AnimId -> f ([Row (Gui Responsive o)] -> Gui Responsive o)
renderRows mParensId =
    do
        vspace <- Spacer.getSpaceSize <&> (^._2)
        -- TODO: better way to make space between rows in grid??
        obox <- Options.box
        pad <- Element.pad
        let -- When there's only "if" and "else", we want to merge the predicate with the keyword
            -- because there are no several predicates to be aligned
            prep2 row =
                row
                & rKeyword .~ obox Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
                & rPredicate .~ Element.empty
        let spaceAbove = (<&> pad (Vector2 0 vspace) 0)
        let prepareRows [] = []
            prepareRows [x, y] = [prep2 x, spaceAbove (prep2 y)]
            prepareRows (x:xs) = x : (xs <&> spaceAbove)
        vert <- verticalRowRender
        addParens <- maybe (pure id) (ResponsiveExpr.addParens ??) mParensId
        vbox <- Responsive.vboxSpaced
        table <- Options.table
        pure $
            \rows ->
            vbox (rows <&> vert)
            & Options.tryWideLayout table (Compose (prepareRows rows))
            & Responsive.rWideDisambig %~ addParens

make ::
    (Monad i, Monad o) =>
    Tree (Sugar.IfElse (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make ifElse pl =
    stdWrapParentExpr pl
    <*> ( renderRows (ExprGui.mParensId pl)
            <*>
            ( (:)
                <$> makeIfThen Element.empty animId ifElse
                <*> makeElse (ifElse ^. Sugar.iElse)
            )
        )
    where
        animId = WidgetIds.fromExprPayload pl & Widget.toAnimId
