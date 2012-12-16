module Graphics.UI.Bottle.View
  ( View, augmentAnimId
  ) where

import qualified Data.ByteString.Char8 as SBS8
import qualified Graphics.UI.Bottle.Animation as Anim

type View = (Anim.Size, Anim.Frame)

augmentAnimId :: Show a => Anim.AnimId -> a -> Anim.AnimId
augmentAnimId animId = Anim.joinId animId . (:[]) . SBS8.pack . show
