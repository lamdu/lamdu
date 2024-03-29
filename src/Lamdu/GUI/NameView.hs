{-# LANGUAGE TypeFamilies #-}

module Lamdu.GUI.NameView
    ( make
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (View, Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.Name as NameTheme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Styled as Styled
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name

import           Lamdu.Prelude

makeCollisionSuffixLabel :: _ => Name.Collision -> m (Maybe (WithTextPos View))
makeCollisionSuffixLabel mCollision =
    case mCollision of
    Name.NoCollision -> pure Nothing
    Name.Collision suffix -> Label.make suffix <&> Just
    Name.UnknownCollision -> Label.make "?" <&> Just

mGlueRight :: (MonadReader env m, Glue.Glue env b a, Glue.Glued a b ~ b) => Maybe a -> b -> m (Glue.Glued a b)
mGlueRight Nothing x = pure x
mGlueRight (Just r) l = Glue.mkGlue Glue.Horizontal l r

make :: _ => Name -> m (WithTextPos View)
make name =
    do
        (Name.TagText visibleName textCollision, tagCollision) <- Name.visible name
        nameTheme <- Lens.view (has . Theme.name)
        mTextSuffixLabel <-
            makeCollisionSuffixLabel textCollision
            >>= Lens._Just (Draw.backgroundColor (nameTheme ^. NameTheme.textCollisionSuffixBGColor))
            & Styled.withColor TextColors.collisionSuffixTextColor
            <&> Lens._Just %~ Aligned 0.5 . Element.scale (nameTheme ^. NameTheme.collisionSuffixScaleFactor) . (^. Align.tValue)
            & local (Element.elemIdPrefix <>~ "text-suffix")
        mTagSuffixLabel <-
            makeCollisionSuffixLabel tagCollision
            & local (Element.elemIdPrefix <>~ "tag-suffix")
        elemId <- Lens.view Element.elemIdPrefix
        TextView.make visibleName elemId
            <&> Aligned 0.5
            >>= mGlueRight mTextSuffixLabel
            <&> (^. Align.value)
            >>= mGlueRight mTagSuffixLabel
