{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Graphics.UI.Bottle.Widgets.EventMapDoc(makeView, addHelp, makeToggledHelpAdder) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Lens ((^.))
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Monoid (mappend)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data Tree n l = Leaf l | Branch n [Tree n l]
  deriving (Eq, Ord, Show, Functor)

bitraverseTree :: Applicative f => (n0 -> f n1) -> (l0 -> f l1) -> Tree n0 l0 -> f (Tree n1 l1)
bitraverseTree _ onLeaf (Leaf l) = Leaf <$> onLeaf l
bitraverseTree onNode onLeaf (Branch n ts) = Branch <$> onNode n <*> traverse (bitraverseTree onNode onLeaf) ts

treeNodes :: Lens.Traversal (Tree n0 l) (Tree n1 l) n0 n1
treeNodes = (`bitraverseTree` pure)

groupTree :: Eq n => [([n], l)] -> [Tree n l]
groupTree = foldr step []
  where
    step ([], l) rest = Leaf l : rest
    step ((at:as), l) b =
      case b of
        Branch bt bs : rest
          | at == bt -> Branch bt (step (as, l) bs) : rest
        _ -> Branch at (step (as, l) []) : b

type View = (Anim.Size, Anim.Frame)

-- We also rely on Map.toList returning a sorted list
groupInputDocs :: [([E.Subtitle], E.InputDoc)] -> [([E.Subtitle], [E.InputDoc])]
groupInputDocs = Map.toList . Map.fromListWith (++) . (map . Lens.over Lens._2) (:[])

addAnimIds :: (Show a, Show b) => AnimId -> Tree a b -> Tree (AnimId, a) (AnimId, b)
addAnimIds animId (Leaf b) = Leaf (animId ++ ["leaf"], b)
addAnimIds animId (Branch a cs) =
  Branch (tAnimId, a) $ map (addAnimIds tAnimId) cs
  where
    tAnimId = TextView.augment animId a

shortcutKeyDocColor :: Draw.Color
shortcutKeyDocColor = Draw.Color 0.1 0.9 0.9 1

makeShortcutKeyView ::
  TextView.Style -> (AnimId, [E.InputDoc]) -> View
makeShortcutKeyView style (animId, inputDocs) =
  GridView.horizontalAlign 0 .
  List.intersperse semicolon .
  map
  ( fgColor shortcutKeyDocColor
  . TextView.label style animId) $
  inputDocs
  where
    semicolon = TextView.label style animId ";"
    fgColor = Lens.over Lens._2 . Anim.onImages . Draw.tint

makeTextViews ::
  TextView.Style -> AnimId ->
  Tree E.Subtitle [E.InputDoc] ->
  Tree View View
makeTextViews style =
  fmap
  ( Lens.over treeNodes (uncurry (TextView.label style))
  . fmap (makeShortcutKeyView style)
  ) . addAnimIds

makeView :: EventMap a -> TextView.Style -> AnimId -> View
makeView eventMap style animId =
  makeTreeView .
  map (makeTextViews style animId) . groupTree . groupInputDocs .
  map (Lens.over Lens._1 E.docStrs . Tuple.swap) $ E.eventMapDocs eventMap

makeTreeView :: [Tree View View] -> View
makeTreeView =
  GridView.makeAlign 0 . recurse
  where
    recurse = concat . map fromTree
    fromTree (Leaf inputDocsView) = [[inputDocsView]]
    fromTree (Branch titleView trees) =
      let row : rows = recurse trees
      in (titleView : Spacer.make 5 : row) :
         map ((Spacer.make 0 :) . (Spacer.make 0 :)) rows

addHelp :: Widget.Size -> TextView.Style -> Widget f -> Widget f
addHelp size style w =
  Lens.over Widget.wFrame (mappend docFrame) w
  where
    (eventMapSize, eventMapDoc) = makeView eventMap style ["help box"]
    transparency = Draw.Color 1 1 1
    docFrame =
      (Anim.onImages . Draw.tint . transparency) 0.8 .
      Anim.onDepth (subtract 10) .
      Anim.translate (size - eventMapSize) .
      Anim.backgroundColor
      ["help doc background"] 1 (Draw.Color 0.3 0.2 0.1 0.5) eventMapSize $
      eventMapDoc
    eventMap = w ^. Widget.wEventMap

makeToggledHelpAdder
  :: [E.ModKey] -> IO (TextView.Style -> Widget.Size -> Widget IO -> IO (Widget IO))
makeToggledHelpAdder overlayDocKeys = do
  showingHelpVar <- newIORef True
  let
    toggle = modifyIORef showingHelpVar not
    addToggleEventMap doc =
      Widget.strongerEvents $
      Widget.keysEventMap overlayDocKeys doc toggle
  return $ \style size widget -> do
    showingHelp <- readIORef showingHelpVar
    return $
      if showingHelp
      then addHelp size style $ addToggleEventMap (E.Doc ["Help", "Key Bindings", "Hide"]) widget
      else addToggleEventMap (E.Doc ["Help", "Key Bindings", "Show"]) widget
