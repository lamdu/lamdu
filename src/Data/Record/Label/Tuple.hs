{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Record.Label.Tuple(first, second) where

import qualified Control.Arrow     as Arrow
import           Data.Record.Label ((:->), lens)

first :: (a, b) :-> a
first = lens fst (Arrow.first . const)

second :: (a, b) :-> b
second = lens snd (Arrow.second . const)
