-- | AnnotationMode for the subtext(annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.AnnotationMode
    ( AnnotationMode(..)
    , initial
    , switchEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.Config as Config

import           Lamdu.Prelude

data AnnotationMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

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
