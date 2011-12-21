{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Record.Label.Maybe(fromMaybe) where

import qualified Data.Maybe        as Maybe
import           Data.Record.Label ((:->), lens)

fromMaybe :: a -> Maybe a :-> a
fromMaybe d = lens (Maybe.fromMaybe d) (const . Just)
