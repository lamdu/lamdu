{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module GUI.Momentu.Element
    ( Element(..), SizedElement(..), Size
    , HasAnimIdPrefix(..), subAnimId, locallyAugmented
    , LayeredImage(..), layers, translateLayeredImage, layeredImageAbove, render
    , pad, padAround
    , topLayer, bottomLayer
    , width, height
    , tint
    , padToSize
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (AnimId, R, Size)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import qualified Graphics.DrawingCombinators as Draw

import           GUI.Momentu.Prelude

-- | LayeredImage is a list of animation frames that overlay on top of each
-- other (first element is most obscured one). When composing Views,
-- the layers at the same list index are composed together and all
-- obscure the layers from a lower index.
newtype LayeredImage = LayeredImage { _layers :: [Anim.Frame] }
Lens.makeLenses ''LayeredImage

instance Semigroup LayeredImage where
    xs <> LayeredImage [] = xs
    LayeredImage [] <> ys = ys
    LayeredImage (x:xs) <> LayeredImage (y:ys) =
        LayeredImage (x<>y : rest ^. layers)
        where
            rest = LayeredImage xs <> LayeredImage ys

instance Monoid LayeredImage where mempty = LayeredImage []

layeredImageAbove :: LayeredImage -> LayeredImage -> LayeredImage
layeredImageAbove (LayeredImage xs) (LayeredImage ys) = LayeredImage (ys ++ xs)

translateLayeredImage :: Vector2 R -> LayeredImage -> LayeredImage
translateLayeredImage pos = layers . traverse %~ Anim.translate pos

render :: LayeredImage -> Anim.Frame
render x = x ^. layers . Lens.reversed . traverse

class Element a where
    setLayeredImage :: Lens.IndexedSetter' Size a LayeredImage
    hoverLayeredImage :: a -> a
    padImpl :: Vector2 R -> Vector2 R -> a -> a
    scale :: Vector2 R -> a -> a
    empty :: a

pad ::
    (MonadReader env m, Element a, Has Dir.Layout env) =>
    m (Vector2 R -> Vector2 R -> a -> a)
pad =
    Lens.view has <&>
    \case
    Dir.LeftToRight -> padImpl
    Dir.RightToLeft ->
        \(Vector2 r t) (Vector2 l b) -> padImpl (Vector2 l t) (Vector2 r b)

-- Different `SetLayeredImage`s do additional things when padding
-- (Moving focal points, alignments, etc)
padAround :: Element a => Vector2 R -> a -> a
padAround p = padImpl p p

class Element a => SizedElement a where size :: Lens.Getter a Size

bottomLayer :: Element a => Lens.IndexedSetter' Size a Anim.Frame
bottomLayer = setLayeredImage <. layers . Lens.ix 0

topLayer :: Element a => Lens.IndexedSetter' Size a Anim.Frame
topLayer = setLayeredImage <. layers . Lens.reversed . Lens.ix 0

tint :: Element a => Draw.Color -> a -> a
tint color = setLayeredImage . layers . traverse . Anim.unitImages %~ Draw.tint color

width :: SizedElement a => Lens.Getter a R
width = size . _1

height :: SizedElement a => Lens.Getter a R
height = size . _2

class HasAnimIdPrefix env where animIdPrefix :: Lens' env AnimId
instance HasAnimIdPrefix [ByteString] where animIdPrefix = id

locallyAugmented ::
    (HasAnimIdPrefix env, MonadReader env m, Show t) => t -> m a -> m a
locallyAugmented x = Reader.local (animIdPrefix %~ Anim.augmentId x)

subAnimId :: (MonadReader env m, HasAnimIdPrefix env) => m (AnimId -> AnimId)
subAnimId = Lens.view animIdPrefix <&> (++)

padToSize ::
    (MonadReader env m, SizedElement a, Has Dir.Layout env) =>
    m (Size -> Vector2 R -> a -> a)
padToSize =
    pad <&> \p newSize alignment x ->
    let sizeDiff = max <$> 0 <*> newSize - x ^. size
    in  p (sizeDiff * alignment) (sizeDiff * (1 - alignment)) x
