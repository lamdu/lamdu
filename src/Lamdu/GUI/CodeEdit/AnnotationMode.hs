-- | AnnotationMode for the subtext(annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.AnnotationMode
    ( AnnotationMode(..)
    , initial
    , switchEventMap
    , statusWidget
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Property as Property
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config

import           Lamdu.Prelude

data AnnotationMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

statusWidget ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Property f AnnotationMode ->
    m (WithTextPos (Widget a))
statusWidget prop =
    (/|/)
    <$> TextView.makeLabel "Annotation "
    <*> TextView.makeLabel (Text.pack (show v))
    <&> Align.tValue %~ Widget.fromView
    where
        v = Property.value prop

initial :: AnnotationMode
initial = Evaluation

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x
    | x == maxBound = minBound
    | otherwise = succ x

nextAnnotationMode :: AnnotationMode -> AnnotationMode
nextAnnotationMode = cyclicSucc

switchEventMap ::
    (Functor f, MonadReader env m, Config.HasConfig env) =>
    Property f AnnotationMode -> m (EventMap (f GuiState.Update))
switchEventMap (Property infoMode setAnnotationMode) =
    Lens.view Config.config <&> \config ->
    E.keysEventMap (Config.nextAnnotationModeKeys config) nextDoc
    (setAnnotationMode next)
    where
        next = nextAnnotationMode infoMode
        nextDoc = E.Doc ["View", "Subtext", "Show " <> Text.pack (show next)]

