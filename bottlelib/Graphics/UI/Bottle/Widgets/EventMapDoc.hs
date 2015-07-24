{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveFunctor #-}
module Graphics.UI.Bottle.Widgets.EventMapDoc
    ( makeView
    , IsHelpShown(..)
    , addHelp
    , makeToggledHelpAdder
    , Config(..)
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Function (on)
import           Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.EventMap (EventMap)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

data Config = Config
    { configStyle :: TextView.Style
    , configInputDocColor :: Draw.Color
    , configBGColor :: Draw.Color
    , configOverlayDocKeys :: [ModKey]
    }

data Tree n l = Leaf l | Branch n [Tree n l]
    deriving (Eq, Ord, Show, Functor)

bitraverseTree :: Applicative f => (n0 -> f n1) -> (l0 -> f l1) -> Tree n0 l0 -> f (Tree n1 l1)
bitraverseTree _ onLeaf (Leaf l) = Leaf <$> onLeaf l
bitraverseTree onNode onLeaf (Branch n ts) = Branch <$> onNode n <*> traverse (bitraverseTree onNode onLeaf) ts

treeNodes :: Lens.Traversal (Tree n0 l) (Tree n1 l) n0 n1
treeNodes = (`bitraverseTree` pure)

groupTree :: Eq node => [([node], leaf)] -> [Tree node leaf]
groupTree = foldr step []
    where
        step ([], l) rest = Leaf l : rest
        step (at:as, l) b =
            case b of
                Branch bt bs : rest
                    | at == bt -> Branch bt (step (as, l) bs) : rest
                _ -> Branch at (step (as, l) []) : b

-- We also rely on Map.toList returning a sorted list
groupInputDocs :: [([E.Subtitle], E.InputDoc)] -> [([E.Subtitle], [E.InputDoc])]
groupInputDocs = Map.toList . Map.fromListWith (++) . (Lens.traversed . _2 %~) (:[])

addAnimIds :: (Show a, Show b) => AnimId -> Tree a b -> Tree (AnimId, a) (AnimId, b)
addAnimIds animId (Leaf b) = Leaf (animId ++ ["leaf"], b)
addAnimIds animId (Branch a cs) =
    Branch (tAnimId, a) $ map (addAnimIds tAnimId) cs
    where
        tAnimId = View.augmentAnimId animId a

makeShortcutKeyView ::
    Config -> (AnimId, [E.InputDoc]) -> View
makeShortcutKeyView config (animId, inputDocs) =
    inputDocs
    <&> TextView.label (configStyle config) animId
    <&> View.animFrame . Anim.unitImages %~ Draw.tint (configInputDocColor config)
    & GridView.verticalAlign 0

makeTextViews ::
    Config -> AnimId ->
    Tree E.Subtitle [E.InputDoc] ->
    Tree View View
makeTextViews config =
    fmap
    ( (treeNodes %~ uncurry (TextView.label (configStyle config)))
    . fmap (makeShortcutKeyView config)
    ) . addAnimIds

addHelpBG :: Config -> AnimId -> View -> View
addHelpBG config animId =
    View.backgroundColor animId 1 $ configBGColor config

columns :: R -> (a -> R) -> [a] -> [[a]]
columns maxHeight itemHeight =
    combine . foldr step (0, [], [])
    where
        combine (_, curColumn, doneColumns) = curColumn : doneColumns
        step new (curColumnHeight, curColumn, doneColumns)
            | newHeight + curColumnHeight > maxHeight =
                (newHeight, [new], curColumn : doneColumns)
            | otherwise =
                (newHeight + curColumnHeight, new : curColumn, doneColumns)
            where
                newHeight = itemHeight new

makeView :: Vector2 R -> EventMap a -> Config -> AnimId -> View
makeView size eventMap config animId =
    makeTreeView config animId size .
    map (makeTextViews config animId) . groupTree . groupInputDocs .
    map ((_1 %~ (^. E.docStrs)) . Tuple.swap) $ E.eventMapDocs eventMap

makeTooltip :: Config -> [ModKey] -> AnimId -> View
makeTooltip config helpKeys animId =
    addHelpBG config animId $
    GridView.horizontalAlign 0
    [ TextView.label (configStyle config) animId "Show help"
    , Spacer.makeHorizontal 10
    , makeShortcutKeyView config
        (animId ++ ["HelpKeys"], map ModKey.pretty helpKeys)
    ]

indent :: R -> View -> View
indent width x =
    GridView.horizontalAlign 0 [Spacer.makeHorizontal width, x]

makeTreeView :: Config -> AnimId -> Vector2 R -> [Tree View View] -> View
makeTreeView config animId size =
    GridView.horizontalAlign 1 . (Lens.traversed %@~ toGrid) .
    columns (size ^. _2) pairHeight .
    handleResult . go
    where
        toGrid i =
            addHelpBG config (View.augmentAnimId animId i) .
            GridView.make . map toRow
        toRow (titleView, docView) = [(0, titleView), (Vector2 1 0, docView)]
        pairHeight (titleView, docView) = (max `on` (^. View.height)) titleView docView
        handleResult (pairs, []) = pairs
        handleResult _ = error "Leafs at root of tree!"
        go = mconcat . map fromTree
        fromTree (Leaf inputDocsView) = ([], [inputDocsView])
        fromTree (Branch titleView trees) =
            ( (titleView, GridView.verticalAlign 1 inputDocs) :
                (Lens.traversed . _1 %~ indent 10) titles
            , [] )
            where
                (titles, inputDocs) = go trees

addHelp :: (AnimId -> View) -> Widget.Size -> Widget f -> Widget f
addHelp f size =
    Widget.animFrame <>~ docFrame
    where
        View eventMapSize eventMapFrame = f ["help box"]
        transparency = Draw.Color 1 1 1
        docFrame =
            eventMapFrame
            & Anim.translate (size - eventMapSize)
            & Anim.layers -~ 10 -- TODO: 10?!
            & Anim.unitImages %~ Draw.tint (transparency 0.8) -- TODO: 0.8?!

data IsHelpShown = HelpShown | HelpNotShown
    deriving (Eq, Ord, Read, Show)

toggle :: IsHelpShown -> IsHelpShown
toggle HelpShown = HelpNotShown
toggle HelpNotShown = HelpShown

makeToggledHelpAdder ::
    IsHelpShown -> IO (Config -> Widget.Size -> Widget IO -> IO (Widget IO))
makeToggledHelpAdder startValue =
    do
        showingHelpVar <- newIORef startValue
        return $ \config size widget ->
            do
                showingHelp <- readIORef showingHelpVar
                let (f, docStr) =
                        case showingHelp of
                        HelpShown ->
                            (makeView size (widget ^. Widget.eventMap) config, "Hide" :: String)
                        HelpNotShown ->
                            (makeTooltip config (configOverlayDocKeys config), "Show")
                    toggleEventMap =
                        Widget.keysEventMap (configOverlayDocKeys config) (E.Doc ["Help", "Key Bindings", docStr]) $
                        modifyIORef showingHelpVar toggle
                return . addHelp f size $
                    Widget.strongerEvents toggleEventMap widget
