{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveFunctor, DeriveTraversable, TemplateHaskell, LambdaCase, TypeFamilies #-}
module Graphics.UI.Bottle.Widgets.EventMapHelp
    ( make
    , IsHelpShown(..)
    , makeToggledHelpAdder
    , Config(..), defaultConfig
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Reader as Reader
import           Data.Function (on)
import           Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Align (Aligned(..))
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.EventMap (EventMap)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), toModKey, noMods)
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.View (View(..), (/|/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

data Config = Config
    { _configStyle :: TextView.Style
    , _configInputDocColor :: Draw.Color
    , _configBGColor :: Draw.Color
    , _configOverlayDocKeys :: [MetaKey]
    , _configTint :: Draw.Color
    }
Lens.makeLenses ''Config

data Env = Env
    { _eConfig :: Config
    , _eAnimIdPrefix :: AnimId
    }
Lens.makeLenses ''Env
instance View.HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance TextView.HasStyle Env where style = eConfig . configStyle

defaultConfig :: Draw.Font -> Config
defaultConfig font =
    Config
    { _configStyle =
        TextView.Style
        { TextView._styleColor = Draw.Color 1 1 1 1
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , _configInputDocColor = Draw.Color 0.1 0.7 0.7 1
    , _configBGColor = Draw.Color 0.2 0.15 0.1 0.5
    , _configOverlayDocKeys = [MetaKey noMods GLFW.Key'F1]
    , _configTint = Draw.Color 1 1 1 0.8
    }

data Tree n l = Leaf l | Branch n [Tree n l]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

addAnimIds :: Show a => AnimId -> Tree a b -> Tree (AnimId, a) (AnimId, b)
addAnimIds animId (Leaf b) = Leaf (animId ++ ["leaf"], b)
addAnimIds animId (Branch a cs) =
    Branch (tAnimId, a) $ map (addAnimIds tAnimId) cs
    where
        tAnimId = Anim.augmentId animId a

makeShortcutKeyView :: [E.InputDoc] -> Env -> View
makeShortcutKeyView inputDocs =
    inputDocs
    <&> (<> " ")
    & traverse TextView.makeLabel
    <&> map (^. Align.tValue)
    <&> Align.vboxAlign 1
    & Reader.local setColor
    where
        setColor env =
            env & TextView.style . TextView.styleColor .~ (env ^. eConfig . configInputDocColor)

makeTextViews ::
    Tree E.Subtitle [E.InputDoc] ->
    Env ->
    Tree View View
makeTextViews tree =
    addAnimIds helpAnimId tree
    & traverse shortcut
    >>= treeNodes mkDoc
    where
        shortcut (animId, doc) =
            makeShortcutKeyView doc
            & Reader.local (View.animIdPrefix .~ animId)
        mkDoc (animId, subtitle) =
            TextView.makeLabel subtitle
            <&> (^. Align.tValue)
            & Reader.local (View.animIdPrefix .~ animId)

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

make :: Vector2 R -> EventMap a -> Env -> View
make size eventMap =
    eventMap ^.. E.emDocs . Lens.withIndex
    <&> (_1 %~ (^. E.docStrs)) . Tuple.swap
    & groupInputDocs & groupTree
    & traverse makeTextViews
    >>= makeTreeView size

makeTooltip :: [ModKey] -> Env -> View
makeTooltip helpKeys env =
    (TextView.makeLabel "Show help" env ^. Align.tValue)
    /|/
    makeShortcutKeyView (helpKeys <&> ModKey.pretty) env

indent :: R -> View -> View
indent width = (Spacer.makeHorizontal width /|/)

fontHeight :: (MonadReader env m, TextView.HasStyle env) => m R
fontHeight =
    Lens.view (TextView.style . TextView.styleFont) <&> Draw.fontHeight

makeFlatTreeView ::
    (MonadReader env m, TextView.HasStyle env) =>
    Vector2 R -> [(View, View)] -> m View
makeFlatTreeView size pairs =
    fontHeight
    <&> Spacer.makeHorizontal
    <&> List.intersperse
    ?? colViews
    <&> Align.hboxAlign 1
    where
        colViews =
            pairs
            & columns (size ^. _2) pairHeight
            <&> map toRow
            <&> GridView.make
        toRow (titleView, docView) = [Aligned 0 titleView, Aligned (Vector2 1 0) docView]
        pairHeight (titleView, docView) = (max `on` (^. View.height)) titleView docView

makeTreeView ::
    (MonadReader env m, TextView.HasStyle env) =>
    Vector2 R -> [Tree View View] -> m View
makeTreeView size trees =
    do
        indentWidth <- fontHeight
        let go ts = ts <&> fromTree & mconcat
            fromTree (Leaf inputDocsView) = ([], [inputDocsView])
            fromTree (Branch titleView ts) =
                ( (titleView, Align.vboxAlign 1 inputDocs) :
                    (Lens.traversed . _1 %~ indent indentWidth) titles
                , [] )
                where
                    (titles, inputDocs) = go ts
        let handleResult (pairs, []) = pairs
            handleResult _ = error "Leafs at root of tree!"
        go trees & handleResult & makeFlatTreeView size

addToBottomRight :: View -> Widget.Size -> View.Layers -> View.Layers
addToBottomRight (View eventMapSize eventMapLayers) size =
    View.addLayersAbove docLayers
    where
        docLayers = View.translateLayers (size - eventMapSize) eventMapLayers

data IsHelpShown = HelpShown | HelpNotShown
    deriving (Eq, Ord, Read, Show)

toggle :: IsHelpShown -> IsHelpShown
toggle HelpShown = HelpNotShown
toggle HelpNotShown = HelpShown

helpAnimId :: AnimId
helpAnimId = ["help box"]

makeToggledHelpAdder ::
    MonadIO m =>
    IsHelpShown ->
    IO
    (Config -> Widget.Size ->
     Widget (m Widget.EventResult) ->
     IO (Widget (m Widget.EventResult)))
makeToggledHelpAdder startValue =
    newIORef startValue
    <&>
    \showingHelpVar config size widget ->
    widget & Widget.wState %%~
    \case
    Widget.StateUnfocused {} -> fail "adding help to non-focused root widget!"
    Widget.StateFocused makeFocus ->
        do
            let env = Env config ["help box"]
            showingHelp <- readIORef showingHelpVar
            return makeFocus
                <&> Lens.mapped %~
                \focus ->
                let (helpView, docStr) =
                        case showingHelp of
                        HelpNotShown ->
                            ( makeTooltip (config ^. configOverlayDocKeys <&> toModKey) env
                            , "Show"
                            )
                        HelpShown ->
                            ( make size
                                ((focus ^. Widget.fEventMap) (Widget.VirtualCursor (focus ^. Widget.fFocalArea)))
                                env
                            , "Hide"
                            )
                    toggleEventMap =
                        Widget.keysEventMap (config ^. configOverlayDocKeys)
                        (E.Doc ["Help", "Key Bindings", docStr]) $
                        liftIO $ modifyIORef showingHelpVar toggle
                    bgHelpView =
                        helpView
                        & View.backgroundColor helpAnimId (config ^. configBGColor)
                        & View.tint (config ^. configTint)
                in
                focus
                & Widget.fLayers %~ addToBottomRight bgHelpView size
                & Widget.fEventMap . Lens.mapped %~ E.strongerEvents toggleEventMap
        <&> Widget.StateFocused
