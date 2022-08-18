module Lamdu.GUI.StatusBar.Sugars
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as BS8
import           Data.Property (Property, pVal, pureModify)
import           GUI.Momentu (noMods)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.State (isSubCursor)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.Sprites as Sprites
import           Lamdu.GUI.StatusBar.Common (StatusWidget)
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import           Lamdu.GUI.Styled (sprite)
import qualified Lamdu.I18N.StatusBar as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Sugar.Config (Sugars)

import           Lamdu.Prelude

sugarFdConfig :: _ => env -> FocusDelegator.Config
sugarFdConfig env = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [noMods ModKey.Key'Enter]
    , FocusDelegator.focusChildDoc = doc Texts.open
    , FocusDelegator.focusParentKeys = [noMods ModKey.Key'Escape]
    , FocusDelegator.focusParentDoc = doc Texts.close
    }
    where
        doc op = E.toDoc env [has . MomentuTexts.edit, has . Texts.enabledSugars, has . op]

make :: _ => Property IO (Sugars Bool) -> m (StatusWidget IO)
make prop =
    ( FocusDelegator.make
    <*> (Lens.view id <&> sugarFdConfig)
    ?? FocusDelegator.FocusEntryParent
    ?? sugarsId
    ) <*>
    do
        top <-
            (Widget.makeFocusableView ?? focusedSugarsId <> Widget.Id ["Header"]) <*> sprite Sprites.sugar
        showMenu <- isSubCursor ?? focusedSugarsId
        if showMenu
            then
                do
                    anchor <- Hover.anchor
                    texts <- Lens.view has
                    hover <- Hover.hover
                    vbox <- Glue.vbox
                    elems <-
                        traverse (uncurry (mkSugarToggle prop)) (((,) <$> texts <*> prop ^. pVal) ^@.. Lens.traversed)
                        <&> Lens.mapped %~ (^. M.tValue)
                    Glue.Poly (///) <- Glue.mkPoly ?? Glue.Vertical
                    let opt x =
                            ( M.Aligned x (anchor top)
                                /// Hover.sequenceHover (hover (vbox (elems <&> M.Aligned x)))
                            ) ^. Align.value
                    top & Hover.hoverInPlaceOf [opt 0, opt 1] . anchor & pure
            else pure top
    <&> (`StatusBar.StatusWidget` mempty) . M.WithTextPos 0

sugarsId :: Widget.Id
sugarsId = Widget.Id ["Sugars"]

focusedSugarsId :: Widget.Id
focusedSugarsId = sugarsId `Widget.joinId` ["inner"]

mkSugarToggle :: _ => Property IO (Sugars Bool) -> Int -> (Text, Bool) -> m (M.TextWidget IO)
mkSugarToggle prop idx (text, val) =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        disabledCol <- Lens.view (has . Theme.disabledColor)
        sbText <- Lens.view (has . Texts.sbStatusBar)
        actionText <- Lens.view (has . if val then Texts.sbDisable else Texts.sbEnable)
        (Widget.makeFocusableView ?? myId <&> (M.tValue %~))
            <*> (TextView.make ?? text ?? Widget.toAnimId myId)
            & (if val then id else local (TextView.color .~ disabledCol))
            <&> M.tValue %~ M.weakerEvents (E.keysEventMap actionKeys (E.Doc [sbText, text, actionText]) toggle)
    & local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = focusedSugarsId <> Widget.Id [BS8.pack (show idx)]
        toggle =
            pureModify prop (Lens.element idx .~ not val)
