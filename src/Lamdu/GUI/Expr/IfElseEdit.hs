{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.Expr.IfElseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import           Data.Functor.Compose (Compose(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar, addValFrame)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Row a = Row
    { _rIndentId :: M.ElemId
    , _rKeyword :: a
    , _rPredicate :: a
    , _rResult :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Row

makeIfThen ::
    _ =>
    M.TextWidget o -> M.ElemId -> ExprGui.Body Sugar.IfElse i o ->
    GuiM env i o (Row (Responsive o))
makeIfThen ifKeyword elemId ifElse =
    do
        env <- Lens.view id
        let jumpToThen =
                ifElse ^?! Sugar.iThen .
                (hVal . Sugar._BodyLam . Sugar.lamFunc . Sugar.fBody . annotation <> annotation)
                & WidgetIds.fromExprPayload & GuiState.updateCursor & pure & const
                & E.charGroup Nothing (E.toDoc env [has . MomentuTexts.navigation, has . Texts.jumpToThen]) ":"
        ifGui <-
            GuiM.makeSubexpression (ifElse ^. Sugar.iIf)
            M./|/ (grammar (Label.make ":") M./|/ Spacer.stdHSpace)
            <&> M.weakerEvents jumpToThen
        thenGui <- GuiM.makeSubexpression (ifElse ^. Sugar.iThen)
        keyword <-
            pure ifKeyword
            M./|/ Spacer.stdHSpace
            <&> Responsive.fromWithTextPos
        Row elemId keyword ifGui thenGui & pure

makeElse :: _ => M.ElemId -> ExprGui.Expr Sugar.Else i o -> GuiM env i o [Row (Responsive o)]
makeElse parentElemId (Ann (Const pl) (Sugar.SimpleElse expr)) =
    Row elseElemId
    <$> (grammar (label Texts.else_) <&> Responsive.fromTextView)
    <*> (grammar (Label.make ":")
            & local (M.elemIdPrefix .~ elseElemId)
            <&> Responsive.fromTextView)
    <*> GuiM.makeSubexpression (Ann (Const pl) expr)
    <&> pure
    where
        elseElemId = parentElemId <> "else"
makeElse _ (Ann (Const pl) (Sugar.ElseIf (Sugar.ElseIfBody addLet content))) =
    do
        -- TODO: green evaluation backgrounds, "◗"?
        letEventMap <- ExprEventMap.addLetEventMap addLet
        elseIfKeyword <-
            label Texts.elseIf
            >>= M.tValue (Widget.makeFocusableView myId)
            >>= M.tValue (ExprEventMap.add ExprEventMap.defaultOptions pl)
            & grammar
        (:)
            <$> ( makeIfThen elseIfKeyword elemId content
                  <&> Lens.mapped %~ M.weakerEvents letEventMap
                )
            <*> makeElse elemId (content ^. Sugar.iElse)
    & local (M.elemIdPrefix .~ elemId)
    where
        myId = WidgetIds.fromEntityId entityId
        elemId = M.asElemId myId
        entityId = pl ^. Sugar.plEntityId

verticalRowRender :: _ => Row (Responsive o) -> f (Responsive o)
verticalRowRender row =
    sequenceA
    [ Options.box Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
    , ResponsiveExpr.indent (row ^. rIndentId) (row ^. rResult)
    ] >>= Responsive.vboxSpaced

renderRows :: _ => Maybe M.ElemId -> [Row (Responsive o)] -> f (Responsive o)
renderRows mParensId rows =
    do
        vspace <- Spacer.getSpaceSize <&> (^._2)
        -- TODO: better way to make space between rows in grid??
        obox <- Options.box Options.disambiguationNone & pushToReader
        let -- When there's only "if" and "else", we want to merge the predicate with the keyword
            -- because there are no several predicates to be aligned
            prep2 row =
                row
                & rKeyword .~ obox [row ^. rKeyword, row ^. rPredicate]
                & rPredicate .~ M.empty
        spaceAbove <- Element.pad (Vector2 0 vspace) 0 & pushToReader
        let prepareRows [] = []
            prepareRows [x, y] = [prep2 x, prep2 y <&> spaceAbove]
            prepareRows (x:xs) = x : (xs <&> Lens.mapped %~ spaceAbove)
        table <- Options.table
        traverse verticalRowRender rows
            >>= Responsive.vboxSpaced
            <&> Options.tryWideLayout table (Compose (prepareRows rows))
            >>= (Responsive.rWide . Responsive.lWideDisambig) (maybe pure ResponsiveExpr.addParens mParensId)

make :: _ => ExprGui.Expr Sugar.IfElse i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) ifElse) =
    do
        ifKeyword <- label Texts.if_ & grammar <&> M.tValue %~ Widget.fromView
        rows <-
            (:)
            <$> makeIfThen ifKeyword elemId ifElse
            <*> makeElse elemId (ifElse ^. Sugar.iElse)
        res <- renderRows (ExprGui.mParensId pl) rows
        frame <- pushToReader addValFrame
        s <- Spacer.stdHSpace <&> Widget.fromView <&> M.WithTextPos 0
        disambig <- maybe (pure id) (ResponsiveExpr.addParens <&> pushToReader) (ExprGui.mParensId pl)
        hbox <- pushToReader Glue.hbox
        let oneLinerLayout (Compose [if_, else_]) Responsive.WideOneLiner =
                Just (Responsive.WideLayouts r (disambig r) Responsive.WideOneLiner)
                where
                    r = if_ ^.. traverse <> [s, else_ ^. rKeyword, else_ ^. rPredicate, s, else_ ^. rResult] & hbox & frame
            oneLinerLayout _ _ = Nothing
        let oneLiner = Options.wideLayoutUnambiguousOption oneLinerLayout
        Options.tryWideLayout oneLiner (Compose rows) res & pure
    & stdWrapParentExpr pl
    where
        elemId = WidgetIds.fromExprPayload pl & M.asElemId
