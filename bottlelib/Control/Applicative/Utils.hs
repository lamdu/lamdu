{-# LANGUAGE NoImplicitPrelude #-}
module Control.Applicative.Utils (when) where

import Prelude.Compat

when :: Applicative f => Bool -> f () -> f ()
when True x = x
when False _ = pure ()
