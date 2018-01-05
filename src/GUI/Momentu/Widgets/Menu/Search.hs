{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module GUI.Momentu.Widgets.Menu.Search
    ( emptyPickEventMap
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widgets.Menu as Menu

import           Lamdu.Prelude

emptyPickEventMap ::
    (MonadReader env m, Menu.HasConfig env, Applicative f) =>
    m (EventMap (f State.Update))
emptyPickEventMap =
    Lens.view Menu.config
    <&> Menu.configKeys
    <&> (Menu.keysPickOption <> Menu.keysPickOptionAndGotoNext)
    <&> \keys -> E.keysEventMap keys (E.Doc ["Pick (N/A)"]) (pure ())
