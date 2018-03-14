-- | AnnotationMode for the subtext(annotations)
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.CodeEdit.AnnotationMode
    ( AnnotationMode(..)
    , initial
    , next
    ) where

import Lamdu.Prelude

data AnnotationMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

initial :: AnnotationMode
initial = Evaluation

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x
    | x == maxBound = minBound
    | otherwise = succ x

next :: AnnotationMode -> AnnotationMode
next = cyclicSucc

