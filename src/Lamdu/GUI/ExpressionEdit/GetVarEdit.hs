module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make, makeGetBinder, makeNoActions, makeSimpleView, addInfixMarker
    , Role(..)
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as SBS8
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Underline(..))
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (Config, HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.NameView as NameView
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeSimpleView ::
    ( MonadReader env m, GuiState.HasCursor env, HasTheme env
    , Applicative f, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , Glue.HasTexts env -- TODO: An artifact constraint
    ) =>
    Lens.ALens' TextColors Draw.Color -> Name x -> Widget.Id ->
    m (TextWidget f)
makeSimpleView color name myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> NameView.make name
    & Styled.withColor (Lens.cloneLens color)

makeParamsRecord ::
    ( MonadReader env m, HasTheme env, GuiState.HasCursor env
    , Element.HasAnimIdPrefix env, Spacer.HasStdSpacing env
    , Texts.HasLanguage env, Applicative f
    ) =>
    Widget.Id -> Sugar.ParamsRecordVarRef (Name f) -> m (Gui Responsive f)
makeParamsRecord myId paramsRecordVar =
    do
        respondToCursor <- Widget.respondToCursorPrefix ?? myId
        (Options.box ?? Options.disambiguationNone)
            <*> sequence
            [ grammar (label (Texts.code . Texts.paramsRecordOpener)) <&> Responsive.fromTextView
            , (Options.boxSpaced ?? Options.disambiguationNone)
              <*>
              ( fieldNames
                & Lens.itraverse
                (\i fieldName ->
                    let paramId = ["params", SBS8.pack (show (i :: Int))]
                    in
                    Widget.joinId myId paramId
                    & makeSimpleView TextColors.parameterColor fieldName
                    <&> Responsive.fromWithTextPos
                    & Reader.local (Element.animIdPrefix %~ (<> paramId))
                )
              )
            , grammar (label (Texts.code . Texts.paramsRecordCloser)) <&> Responsive.fromTextView
            ] <&> respondToCursor
    where
        Sugar.ParamsRecordVarRef fieldNames = paramsRecordVar

infixMarker :: Vector2 Anim.R -> Draw.Image
infixMarker (Vector2 w h) =
    () <$
    mconcat
    [ Draw.line (x, 0) (0,x)
    , Draw.line (w-x, 0) (w,x)
    , Draw.line (w-x, h) (w,h-x)
    , Draw.line (x, h) (0,h-x)
    , Draw.line (0, x) (0, h-x)
    , Draw.line (w, x) (w, h-x)
    , Draw.line (x, 0) (w-x, 0)
    , Draw.line (x, h) (w-x, h)
    ]
    where
        x = min w h / 4

addInfixMarker :: Element a => Widget.Id -> a -> a
addInfixMarker widgetId =
    Element.bottomLayer %@~
    \size -> Anim.singletonFrame 1 frameId (infixMarker size) & flip mappend
    where
        frameId = Widget.toAnimId widgetId ++ ["infix"]

data Role = Normal | Infix deriving Eq

makeNameRef ::
    (Monad i, Monad o) =>
    Role ->
    Lens.ALens' TextColors Draw.Color -> Widget.Id ->
    Sugar.NameRef (Name x) o ->
    ExprGuiM i o (TextWidget o)
makeNameRef role color myId nameRef =
    do
        savePrecursor <- ExprGuiM.mkPrejumpPosSaver
        config <- Lens.view Config.config
        let jumpToDefinitionEventMap =
                E.keysEventMapMovesCursor
                (config ^. Config.jumpToDefinitionKeys ++
                 config ^. Config.extractKeys)
                (E.Doc ["Navigation", "Jump to definition"]) $
                do
                    savePrecursor
                    nameRef ^. Sugar.nrGotoDefinition <&> WidgetIds.fromEntityId
        makeSimpleView color name nameId
            <&> mAddMarker
            <&> Align.tValue %~ Widget.weakerEvents jumpToDefinitionEventMap
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId nameId)
    & GuiState.assignCursor myId nameId
    where
        name = nameRef ^. Sugar.nrName
        nameId = Widget.joinId myId ["name"]
        nameText = Name.visible name ^. _1 . Name.ttText
        mAddMarker
            | (role == Infix) == Lens.allOf Lens.each (`elem` Chars.operator) nameText = id
            | otherwise = addInfixMarker nameId

makeInlineEventMap ::
    Applicative f =>
    Config -> Sugar.BinderVarInline f ->
    Gui EventMap f
makeInlineEventMap config (Sugar.InlineVar inline) =
    inline <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (config ^. Config.inlineKeys)
      (E.Doc ["Edit", "Inline"])
makeInlineEventMap config (Sugar.CannotInlineDueToUses (x:_)) =
    WidgetIds.fromEntityId x & pure
    & E.keysEventMapMovesCursor (config ^. Config.inlineKeys)
      (E.Doc ["Navigation", "Jump to next use"])
makeInlineEventMap _ _ = mempty

definitionTypeChangeBox ::
    ( MonadReader env m
    , Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, HasTheme env, GuiState.HasCursor env
    , HasConfig env, Texts.HasLanguage env
    , Applicative f
    ) =>
    Sugar.DefinitionOutdatedType (Name x) (f Sugar.EntityId) -> Widget.Id ->
    m (TextWidget f)
definitionTypeChangeBox info getVarId =
    do
        oldTypeRow <- Styled.info (label (Texts.codeUI . Texts.defUpdateWas))
        newTypeRow <-
            Styled.actionable myId (Texts.codeUI . Texts.defUpdateHeader)
            updateDoc update
            /|/ Spacer.stdHSpace
            /|/ Styled.info (label (Texts.codeUI . Texts.defUpdateTo))

        oldTypeView <- mkTypeView "oldTypeView" (info ^. Sugar.defTypeWhenUsed)
        newTypeView <- mkTypeView "newTypeView" (info ^. Sugar.defTypeCurrent)

        Grid.make ??
            [ [ Align.fromWithTextPos 0 (oldTypeRow <&> Widget.fromView)
              , Align.fromWithTextPos 0 (oldTypeView <&> Widget.fromView) ]
            , [ Align.fromWithTextPos 0 newTypeRow
              , Align.fromWithTextPos 0 (newTypeView <&> Widget.fromView) ]
            ] <&> snd <&> Align.WithTextPos 0
    where
        update = info ^. Sugar.defTypeUseCurrent <&> WidgetIds.fromEntityId
        updateDoc = E.Doc ["Edit", "Update definition type"]
        mkTypeView idSuffix scheme =
            TypeView.makeScheme scheme
            & Reader.local (Element.animIdPrefix .~ animId ++ [idSuffix])
        myId = Widget.joinId getVarId ["type change"]
        animId = Widget.toAnimId myId

processDefinitionWidget ::
    ( MonadReader env m, Spacer.HasStdSpacing env
    , HasTheme env, Element.HasAnimIdPrefix env, HasConfig env
    , GuiState.HasCursor env, Hover.HasStyle env
    , Texts.HasLanguage env, Applicative f
    ) =>
    Sugar.DefinitionForm (Name x) f -> Widget.Id ->
    m (TextWidget f) ->
    m (TextWidget f)
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted _myId mkLayout =
    Styled.deletedUse <*> mkLayout
processDefinitionWidget (Sugar.DefTypeChanged info) myId mkLayout =
    do
        theme <- Lens.view Theme.theme
        let underline = Underline
                { _underlineColor = theme ^. Theme.errorColor
                , _underlineWidth = theme ^. Theme.wideUnderlineWidth
                }
        layout <-
            Reader.local (TextView.underline ?~ underline) mkLayout
            & GuiState.assignCursor hiddenId myId
        isSelected <- GuiState.isSubCursor ?? myId
        isHidden <- GuiState.isSubCursor ?? hiddenId
        case (isHidden, isSelected) of
            (True, _) ->
                layout
                <&> Widget.strongerEventsWithoutPreevents showDialogEventMap
                & pure
            (False, True) ->
                ( Hover.hoverBeside Align.tValue ?? layout )
                <*>
                ( definitionTypeChangeBox info myId <&> (^. Align.tValue) )
                <&> fmap (Widget.weakerEventsWithoutPreevents hideDialogEventMap)
            (False, False) -> pure layout
    where
        showDialogEventMap =
            pure myId
            & E.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Enter]
            (E.Doc ["View", "Type update dialog", "Show"])
        hideDialogEventMap =
            pure hiddenId
            & E.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Escape]
            (E.Doc ["View", "Type update dialog", "Hide"])
        hiddenId = myId `Widget.joinId` ["hidden"]

makeGetBinder ::
    (Monad i, Monad o) =>
    Role -> Sugar.BinderVarRef (Name x) o -> Widget.Id ->
    ExprGuiM i o (TextWidget o)
makeGetBinder role binderVar myId =
    do
        config <- Lens.view Config.config
        let (color, processDef) =
                case binderVar ^. Sugar.bvForm of
                Sugar.GetLet -> (TextColors.letColor, id)
                Sugar.GetDefinition defForm ->
                    ( TextColors.definitionColor
                    , processDefinitionWidget defForm myId
                    )
        makeNameRef role color myId (binderVar ^. Sugar.bvNameRef)
            <&> Align.tValue %~ Widget.weakerEvents
                (makeInlineEventMap config (binderVar ^. Sugar.bvInline))
            & processDef

makeGetParam ::
    (Monad i, Monad o) =>
    Sugar.ParamRef (Name x) o -> Widget.Id ->
    ExprGuiM i o (TextWidget o)
makeGetParam param myId =
    do
        theme <- Lens.view Theme.theme
        let mk = makeNameRef Normal TextColors.parameterColor myId (param ^. Sugar.pNameRef)
        case param ^. Sugar.pBinderMode of
            Sugar.LightLambda ->
                mk
                & Reader.local (TextView.underline ?~ LightLambda.underline theme)
                & Styled.nameAtBinder name
            Sugar.NormalBinder -> mk
    where
        name = param ^. Sugar.pNameRef . Sugar.nrName

makeNoActions ::
    (Monad i, Monad o) =>
    Sugar.GetVar (Name o) o ->
    Widget.Id ->
    ExprGuiM i o (Gui Responsive o)
makeNoActions getVar myId =
    case getVar of
    Sugar.GetBinder binderVar ->
        makeGetBinder Normal binderVar myId <&> Responsive.fromWithTextPos
    Sugar.GetParamsRecord paramsRecordVar ->
        makeParamsRecord myId paramsRecordVar
    Sugar.GetParam param ->
        makeGetParam param myId <&> Responsive.fromWithTextPos

make ::
    (Monad i, Monad o) =>
    Sugar.GetVar (Name o) o ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make getVar pl =
    stdWrap pl <*> makeNoActions getVar (WidgetIds.fromExprPayload pl)
