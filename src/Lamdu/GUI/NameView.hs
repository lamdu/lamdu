module Lamdu.GUI.NameView
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.Name as NameTheme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name

import           Lamdu.Prelude

makeCollisionSuffixLabel ::
    ( MonadReader env m, Has Dir.Layout env
    , Has TextView.Style env, Element.HasAnimIdPrefix env, Has Theme env
    ) => Lens.ALens' NameTheme.Name Draw.Color -> Name.Collision -> m (Maybe View)
makeCollisionSuffixLabel collisionColor mCollision =
    case mCollision of
    Name.NoCollision -> pure Nothing
    Name.Collision suffix -> mk suffix
    Name.UnknownCollision -> mk "?"
    where
        mk text =
            do
                nameTheme <- Lens.view (has . Theme.name)
                (Draw.backgroundColor ?? nameTheme ^# collisionColor)
                    <*>
                    (Label.make text
                     & Styled.withColor TextColors.collisionSuffixTextColor
                     <&> Element.scale (nameTheme ^. NameTheme.collisionSuffixScaleFactor))
            <&> (^. Align.tValue)
            <&> Just

make ::
    ( MonadReader env m
    , Has Theme env, Element.HasAnimIdPrefix env, Has TextView.Style env
    , Has Dir.Layout env, Has (Texts.Name Text) env
    ) =>
    Name -> m (WithTextPos View)
make name =
    do
        (Name.TagText visibleName textCollision, tagCollision) <- Name.visible name
        mTextSuffixLabel <-
            makeCollisionSuffixLabel NameTheme.textCollisionSuffixBGColor textCollision
            <&> Lens._Just %~ Aligned 0.5
            & Reader.local (Element.animIdPrefix <>~ ["text-suffix"])
        mTagSuffixLabel <-
            makeCollisionSuffixLabel NameTheme.tagCollisionSuffixBGColor tagCollision
            <&> Lens._Just %~ Aligned 0.5
            & Reader.local (Element.animIdPrefix <>~ ["tag-suffix"])
        animId <- Lens.view Element.animIdPrefix
        (|||) <- Glue.mkGlue ?? Glue.Horizontal
        TextView.make ?? visibleName ?? animId
            <&> Aligned 0.5
            <&> maybe id (flip (|||)) mTextSuffixLabel
            <&> maybe id (flip (|||)) mTagSuffixLabel
            <&> (^. Align.value)
