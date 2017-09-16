{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.NameEdit
    ( makeView
    , makeBareEdit
    , makeAtBinder, styleNameAtBinder
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Names.Types as Name

import           Lamdu.Prelude

type T = Transaction

disallowedNameChars :: String
disallowedNameChars = "[]\\`()"

nameEditFDConfig :: FocusDelegator.Config
nameEditFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Rename"]
    , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Done renaming"]
    }

-- TODO: This doesn't belong here
makeCollisionSuffixLabel ::
    ( TextView.HasStyle r, Element.HasAnimIdPrefix r, HasTheme r
    , MonadReader r m
    ) => Name.Collision -> m (Maybe View)
makeCollisionSuffixLabel mCollision =
    case mCollision of
    Name.NoCollision -> return Nothing
    Name.Collision suffix -> mk (Text.pack (show suffix))
    Name.UnknownCollision -> mk "?"
    where
        mk text =
            do
                th <- Lens.view theme
                let Theme.Name{..} = Theme.name th
                (Draw.backgroundColor ?? collisionSuffixBGColor)
                    <*>
                    (TextView.makeLabel text
                     & Reader.local (TextView.color .~ collisionSuffixTextColor)
                     <&> Element.scale (realToFrac <$> collisionSuffixScaleFactor))
            <&> (^. Align.tValue)
            <&> Just

-- TODO: This doesn't belong here
makeView ::
    ( HasTheme r, Element.HasAnimIdPrefix r
    , TextView.HasStyle r, MonadReader r m
    ) => Name.Form -> AnimId -> m (WithTextPos View)
makeView name animId =
    do
        mSuffixLabel <-
            makeCollisionSuffixLabel mCollision <&> Lens._Just %~ Aligned 0.5
        TextView.make ?? visibleName ?? animId
            <&> Aligned 0.5
            <&> maybe id (flip (/|/)) mSuffixLabel
            <&> (^. Align.value)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        (visibleName, mCollision) = Name.visible name

-- | A name edit without the collision suffixes
makeBareEdit ::
    (Monad m, TextEdit.HasStyle env, Widget.HasCursor env, MonadReader env f) =>
    Name m -> Widget.Id ->
    f (WithTextPos (Widget (T m Widget.EventResult)))
makeBareEdit (Name form setName) myId =
    TextEdits.makeWordEdit
    ?? TextEdit.EmptyStrings visibleName ""
    ?? Property storedName setName
    ?? myId
    <&> Align.tValue . E.eventMap %~ E.filterChars (`notElem` disallowedNameChars)
    where
        (visibleName, _mCollision) = Name.visible form
        storedName = form ^. Name._Stored . _1

make ::
    ( Monad m
    , MonadReader env f, TextEdit.HasStyle env, Element.HasAnimIdPrefix env
    , HasTheme env, Widget.HasCursor env
    ) => Name m -> Widget.Id ->
    f (WithTextPos (Widget (T m Widget.EventResult)))
make name myId =
    do
        mCollisionSuffix <- makeCollisionSuffixLabel mCollision
        makeBareEdit name myId
            <&> case mCollisionSuffix of
                Nothing -> id
                Just collisionSuffix ->
                    \nameEdit ->
                        (Aligned 0.5 nameEdit /|/ Aligned 0.5 collisionSuffix)
                        ^. Align.value
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        (_visibleName, mCollision) = name ^. Name.form & Name.visible

styleNameAtBinder ::
    ( MonadReader env m
    , Style.HasStyle env
    ) => Name n -> Draw.Color -> m b -> m b
styleNameAtBinder name color act =
    do
        style <- Lens.view Style.style
        let textEditStyle =
                style
                ^. case name ^. Name.form of
                    Name.AutoGenerated {} -> Style.styleAutoNameOrigin
                    Name.Unnamed {}       -> Style.styleAutoNameOrigin
                    Name.Stored {}        -> Style.styleNameAtBinder
                & TextEdit.sTextViewStyle . TextView.styleColor .~ color
        act & Reader.local (TextEdit.style .~ textEditStyle)

makeAtBinder ::
    (Monad m, MonadReader env f, Widget.HasCursor env, HasTheme env
    , Element.HasAnimIdPrefix env, Style.HasStyle env
    ) => Name m -> Draw.Color -> Widget.Id ->
    f (WithTextPos (Widget (T m Widget.EventResult)))
makeAtBinder name color myId =
    ( FocusDelegator.make ?? nameEditFDConfig
      ?? FocusDelegator.FocusEntryParent ?? myId
      <&> (Align.tValue %~)
    ) <*> make name (WidgetIds.nameEditOf myId)
    & styleNameAtBinder name color
