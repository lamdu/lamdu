{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Responsive.TaggedList
    ( TaggedItem(..), tagPre, taggedItem, tagPost
    , taggedList
    ) where

import qualified Control.Lens.Extended as Lens
import           Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.State (Gui)
import           GUI.Momentu.Responsive
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           Lamdu.Prelude

data TaggedItem a = TaggedItem
    { _tagPre :: Maybe (WithTextPos (Widget a))
    , _taggedItem :: Responsive a
    , _tagPost :: Maybe (WithTextPos (Widget a))
    } deriving Functor

Lens.makeLenses ''TaggedItem

taggedList ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Applicative f
    , Glue.HasTexts env
    ) =>
    m ([Gui TaggedItem f] -> Gui Responsive f)
taggedList =
    (,,,)
    <$> Element.pad
    <*> (Glue.mkGlue ?? Glue.Horizontal)
    <*> Glue.vbox
    <*> (Spacer.stdVSpace <&> Widget.fromView <&> WithTextPos 0)
    <&>
    \(doPad, (/|/), vboxed, vspace) items ->
    let preWidth = items ^.. traverse . tagPre . Lens._Just . Element.width & maximum
        postWidth = items ^.. traverse . tagPost . Lens._Just . Element.width & maximum
        renderItem ((Nothing, post), item) = (item, post)
        renderItem ((Just pre, post), item) =
            ( doPad (Vector2 (preWidth - pre ^. Element.width) 0) 0 pre
                /|/ item
            , post
            )
        renderItems xs =
            xs <&> renderRow & List.intersperse vspace & vboxed
            where
                renderRow (item, Nothing) = item
                renderRow (item, Just post) =
                    item /|/
                    doPad (Vector2 (itemWidth - item ^. Element.width) 0) 0 post
                itemWidth =
                    xs ^.. traverse . Lens.filteredBy (_2 . Lens._Just) . _1 . Element.width
                    & maximum
        idx =
            NarrowLayoutParams
            { _layoutWidth = preWidth + postWidth
            , _layoutNeedDisambiguation = False
            }
    in
    items <&> prepItem & Compose
    & verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout = renderItems . map renderItem . getCompose
    }
    where
        prepItem (TaggedItem pre x post) = ((pre, post), x)
