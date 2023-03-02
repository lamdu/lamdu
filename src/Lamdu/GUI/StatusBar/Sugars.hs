module Lamdu.GUI.StatusBar.Sugars
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property, pVal, pureModify)
import           GUI.Momentu (noMods)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
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
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.StatusBar as Texts
import           Lamdu.Sugar.Config (Sugars)

import           Lamdu.Prelude
import Control.Monad.Reader.Extended (pushToReader)

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
    do
        top <- sprite Sprites.sugar >>= Widget.makeFocusableView (focusedSugarsId <> "Header")
        showMenu <- isSubCursor focusedSugarsId
        c <- Lens.view id <&> sugarFdConfig
        ( if showMenu
            then
                do
                    anchor <- pushToReader Hover.anchor
                    texts <- Lens.view has
                    hover <- pushToReader Hover.hover
                    vbox <- pushToReader Glue.vbox
                    elems <-
                        traverse (uncurry (mkSugarToggle prop)) (((,) <$> texts <*> prop ^. pVal) ^@.. Lens.traversed)
                        <&> Lens.mapped %~ (^. M.tValue)
                    Glue.Poly (///) <- Glue.mkPoly Glue.Vertical
                    let opt x =
                            ( M.Aligned x (anchor top)
                                /// Hover.sequenceHover (hover (vbox (elems <&> M.Aligned x)))
                            ) ^. Align.value
                    top & Hover.hoverInPlaceOf [opt 0, opt 1] . anchor & pure
            else pure top
            ) >>= FocusDelegator.make c FocusDelegator.FocusEntryParent sugarsId
    <&> (`StatusBar.StatusWidget` mempty) . M.WithTextPos 0

sugarsId :: ElemId
sugarsId = "Sugars"

focusedSugarsId :: ElemId
focusedSugarsId = sugarsId <> "inner"

mkSugarToggle :: _ => Property IO (Sugars Bool) -> Int -> (Text, Bool) -> m (M.TextWidget IO)
mkSugarToggle prop idx (text, val) =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        disabledCol <- Lens.view (has . Theme.disabledColor)
        sbText <- Lens.view (has . Texts.sbStatusBar)
        actionText <- Lens.view (has . if val then Texts.sbDisable else Texts.sbEnable)
        TextView.make text myId >>= M.tValue (Widget.makeFocusableView myId)
            & (if val then id else local (TextView.color .~ disabledCol))
            <&> M.tValue %~ M.weakerEvents (E.keysEventMap actionKeys (E.Doc [sbText, text, actionText]) toggle)
    & local (Element.elemIdPrefix .~ myId)
    where
        myId = focusedSugarsId <> M.asElemId idx
        toggle =
            pureModify prop (Lens.element idx .~ not val)
