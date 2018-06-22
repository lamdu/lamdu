-- | AnnotationMode for the subtext(annotations)
module Lamdu.GUI.CodeEdit.AnnotationMode
    ( AnnotationMode(..)
    , initial
    ) where

import Lamdu.Prelude

data AnnotationMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

initial :: AnnotationMode
initial = Evaluation
