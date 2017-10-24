{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Choice widget for presentation mode

module Lamdu.GUI.PresentationModeEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import qualified Data.Text as Text
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

presentationModeChoiceConfig :: Choice.Config
presentationModeChoiceConfig = Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Presentation Mode", "Select"]
        , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Presentation Mode", "Choose selected"]
        }
    , Choice.cwcOrientation = Choice.Vertical
    , Choice.cwcExpandMode = Choice.ExplicitEntry
    }

{-# ANN make ("HLint: ignore Use head"::String) #-}

make ::
    ( Monad m, MonadReader env n, HasTheme env
    , Element.HasAnimIdPrefix env, TextView.HasStyle env, Widget.HasCursor env
    ) =>
    Widget.Id ->
    Sugar.BinderParams name m ->
    Property m Sugar.PresentationMode ->
    n (Widget (m State.Update))
make myId (Sugar.FieldParams params) prop =
    do
        theme <- Lens.view Theme.theme
        pairs <-
            traverse mkPair [Sugar.Object (paramTags !! 0), Sugar.Verbose, Sugar.Infix (paramTags !! 0) (paramTags !! 1)]
            & Reader.local
                (TextView.style . TextView.styleColor .~ Theme.presentationChoiceColor (Theme.codeForegroundColors theme))
        Choice.make ?? Property.set prop ?? pairs ?? cur
            ?? presentationModeChoiceConfig ?? myId
            <&> Element.scale (realToFrac <$> Theme.presentationChoiceScaleFactor theme)
    where
        cur = Property.value prop
        paramTags = params ^.. traverse . Sugar.fpInfo . Sugar.fpiTag . Sugar.tagInfo . Sugar.tagVal
        mkPair presentationMode =
            TextView.makeFocusableLabel text <&> (^. Align.tValue)
            <&> (,) presentationMode
            where
                text =
                    case presentationMode of
                    Sugar.Verbose -> "Verbose"
                    Sugar.Object{} -> "OO"
                    Sugar.Infix{} -> "Infix"
                    & Text.pack
make _ _ _ =
    -- This shouldn't happen?
    return Element.empty
