{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.Settings
    ( Settings(..), sAnnotationMode, AnnotationMode(..), defaultAnnotationMode
    , HasSettings(..)
    , nextAnnotationMode
    , mkEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.IORef
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config

import           Lamdu.Prelude

data AnnotationMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

defaultAnnotationMode :: AnnotationMode
defaultAnnotationMode = Evaluation

newtype Settings = Settings
    { _sAnnotationMode :: AnnotationMode
    }
Lens.makeLenses ''Settings

class HasSettings env where settings :: Lens' env Settings

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x
    | x == maxBound = minBound
    | otherwise = succ x

nextAnnotationMode :: AnnotationMode -> AnnotationMode
nextAnnotationMode = cyclicSucc

mkEventMap ::
    (Settings -> IO ()) -> Config -> IORef Settings ->
    IO (EventMap (IO GuiState.Update))
mkEventMap onSettingsChange config settingsRef =
    do
        theSettings <- readIORef settingsRef
        let next = theSettings ^. sAnnotationMode & nextAnnotationMode
        let nextDoc = E.Doc ["View", "Subtext", "Show " <> Text.pack (show next)]
        let nextSettings = theSettings & sAnnotationMode .~ next
        do
            writeIORef settingsRef nextSettings
            onSettingsChange nextSettings
            & E.keysEventMap (Config.nextAnnotationModeKeys config) nextDoc
            & pure
