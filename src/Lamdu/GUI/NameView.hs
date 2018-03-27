{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.NameView
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.Name as NameTheme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Styled as Styled
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name

import           Lamdu.Prelude

makeCollisionSuffixLabel ::
    ( TextView.HasStyle r, Element.HasAnimIdPrefix r, HasTheme r
    , MonadReader r m
    ) => Lens.ALens' NameTheme.Name Draw.Color -> Name.Collision -> m (Maybe View)
makeCollisionSuffixLabel collisionColor mCollision =
    case mCollision of
    Name.NoCollision -> pure Nothing
    Name.Collision suffix -> mk (Text.pack (show suffix))
    Name.UnknownCollision -> mk "?"
    where
        mk text =
            do
                nameTheme <- Lens.view (theme . Theme.name)
                (Draw.backgroundColor ?? nameTheme ^# collisionColor)
                    <*>
                    (TextView.makeLabel text
                     & Styled.withColor TextColors.collisionSuffixTextColor
                     <&> Element.scale (nameTheme ^. NameTheme.collisionSuffixScaleFactor))
            <&> (^. Align.tValue)
            <&> Just

make ::
    (HasTheme r, Element.HasAnimIdPrefix r, TextView.HasStyle r, MonadReader r m) =>
    Name f -> m (WithTextPos View)
make name =
    do
        mTextSuffixLabel <-
            makeCollisionSuffixLabel NameTheme.textCollisionSuffixBGColor textCollision
            <&> Lens._Just %~ Aligned 0.5
        mTagSuffixLabel <-
            makeCollisionSuffixLabel NameTheme.tagCollisionSuffixBGColor tagCollision
            <&> Lens._Just %~ Aligned 0.5
        animId <- Element.subAnimId ["name"]
        TextView.make ?? visibleName ?? animId
            <&> Aligned 0.5
            <&> maybe id (flip (/|/)) mTextSuffixLabel
            <&> maybe id (flip (/|/)) mTagSuffixLabel
            <&> (^. Align.value)
    where
        (Name.TagText visibleName textCollision, tagCollision) = Name.visible name
