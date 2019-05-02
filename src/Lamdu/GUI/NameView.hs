{-# LANGUAGE TypeOperators #-}
module Lamdu.GUI.NameView
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widgets.Label as Label
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
    ( MonadReader env m
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env
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
                    (Label.make text
                     & Styled.withColor TextColors.collisionSuffixTextColor
                     <&> Element.scale (nameTheme ^. NameTheme.collisionSuffixScaleFactor))
            <&> (^. Align.tValue)
            <&> Just

make ::
    ( MonadReader env m
    , HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , Glue.HasTexts env -- TODO: This is unused - an artifact constraint
    ) =>
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
        (|||) <- Glue.mkGlue ?? Glue.Horizontal
        TextView.make ?? visibleName ?? animId
            <&> Aligned 0.5
            <&> maybe id (flip (|||)) mTextSuffixLabel
            <&> maybe id (flip (|||)) mTagSuffixLabel
            <&> (^. Align.value)
    where
        (Name.TagText visibleName textCollision, tagCollision) = Name.visible name
