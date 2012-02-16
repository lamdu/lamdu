{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances,
             TupleSections #-}
module Graphics.UI.Bottle.Theme(Theme(..)) where

import qualified Graphics.DrawingCombinators as Draw

data Theme = Theme {
  themeFont :: Draw.Font,
  themeFontSize :: Int,
  themeEmptyString :: String
  }
