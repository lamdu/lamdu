module Control.Applicative.Utils (when) where

import Control.Applicative (Applicative(..))

when :: Applicative f => Bool -> f () -> f ()
when True x = x
when False _ = pure ()
