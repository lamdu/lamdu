module Lamdu.GUI.TypeView
    ( makeScheme, addTypeBG
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (WithTextPos, View)
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

newtype Prec = Prec Int deriving stock (Eq, Ord, Show)

addTypeBG :: _ => a -> m a
addTypeBG view =
    do
        color <- Lens.view (has . Theme.typeFrameBGColor)
        bgId <- Element.subAnimId ?? ["bg"]
        view
            & MDraw.backgroundColor bgId color
            & pure

makeScheme :: _ => Sugar.Scheme Name -> m (WithTextPos View)
makeScheme _ = pure Element.empty
