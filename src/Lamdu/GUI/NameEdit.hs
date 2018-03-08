{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.NameEdit
    ( makeView
    , makeBareEdit
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import           Revision.Deltum.Property (Property(..))

import           Lamdu.Prelude

disallowedNameChars :: String
disallowedNameChars = "[]\\`()"

-- TODO: This doesn't belong here
makeCollisionSuffixLabel ::
    ( TextView.HasStyle r, Element.HasAnimIdPrefix r, HasTheme r
    , MonadReader r m
    ) => (Theme.Name -> Draw.Color) -> Name.Collision -> m (Maybe View)
makeCollisionSuffixLabel collisionColor mCollision =
    case mCollision of
    Name.NoCollision -> pure Nothing
    Name.Collision suffix -> mk (Text.pack (show suffix))
    Name.UnknownCollision -> mk "?"
    where
        mk text =
            do
                nameTheme <- Lens.view theme <&> Theme.name
                (Draw.backgroundColor ?? collisionColor nameTheme)
                    <*>
                    (TextView.makeLabel text
                     & Reader.local (TextView.color .~ Theme.collisionSuffixTextColor nameTheme)
                     <&> Element.scale (Theme.collisionSuffixScaleFactor nameTheme))
            <&> (^. Align.tValue)
            <&> Just

-- TODO: This doesn't belong here
makeView ::
    (HasTheme r, Element.HasAnimIdPrefix r, TextView.HasStyle r, MonadReader r m) =>
    Name f -> m (WithTextPos View)
makeView name =
    do
        mTextSuffixLabel <-
            makeCollisionSuffixLabel Theme.textCollisionSuffixBGColor textCollision
            <&> Lens._Just %~ Aligned 0.5
        mTagSuffixLabel <-
            makeCollisionSuffixLabel Theme.tagCollisionSuffixBGColor tagCollision
            <&> Lens._Just %~ Aligned 0.5
        animId <- Element.subAnimId ["name"]
        TextView.make ?? visibleName ?? animId
            <&> Aligned 0.5
            <&> maybe id (flip (/|/)) mTextSuffixLabel
            <&> maybe id (flip (/|/)) mTagSuffixLabel
            <&> (^. Align.value)
    where
        (Name.TagText visibleName textCollision, tagCollision) = Name.visible name

-- | A name edit without the collision suffixes
makeBareEdit ::
    ( Applicative f, MonadReader env m
    , TextEdit.HasStyle env, GuiState.HasCursor env
    ) =>
    Name.StoredName f -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeBareEdit (Name.StoredName setName tagText _tagCollision storedText) myId =
    TextEdits.makeWordEdit
    ?? TextEdit.EmptyStrings (tagText ^. Name.ttText) ""
    ?? Property storedText setName
    ?? myId
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem` disallowedNameChars)
