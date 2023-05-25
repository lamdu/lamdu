{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module Lamdu.I18N.Texts
    ( Texts(..)
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Zoom as Zoom
import           Lamdu.I18N.Code
import           Lamdu.I18N.CodeUI (CodeUI)
import           Lamdu.I18N.Collaboration
import           Lamdu.I18N.Definitions
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Name
import           Lamdu.I18N.Navigation
import           Lamdu.I18N.StatusBar
import           Lamdu.I18N.Versioning
import           Lamdu.Sugar.Config (Sugars)

import           Lamdu.Prelude

data Texts a = Texts
    { _languageNames :: Map LangId a
    , _code :: Code a
    , _codeUI :: CodeUI a
    , _sugars :: Sugars a
    , _commonTexts :: MomentuTexts.Texts a
    , _collaborationTexts :: Collaboration a
    , _navigationTexts :: Navigation a
    , _definitions :: Definitions a
    , _name :: Name a
    , _statusBar :: StatusBar a
    , _versioning :: Versioning a
    , _dir :: Dir.Texts a
    , _glue :: Glue.Texts a
    , _menu :: Menu.Texts a
    , _searchMenu :: SearchMenu.Texts a
    , _grid :: Grid.Texts a
    , _eventMap :: EventMap.Texts a
    , _dropDownList :: DropDownList.Texts a
    , _textEdit :: TextEdit.Texts a
    , _zoom :: Zoom.Texts a
    , _mainLoop :: MainLoop.Texts a
    }
    deriving Eq
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

instance Has (Map LangId         Text) (Texts Text) where has = languageNames
instance Has (EventMap.Texts     Text) (Texts Text) where has = eventMap
instance Has (Dir.Texts          Text) (Texts Text) where has = dir
instance Has (Glue.Texts         Text) (Texts Text) where has = glue
instance Has (Menu.Texts         Text) (Texts Text) where has = menu
instance Has (SearchMenu.Texts   Text) (Texts Text) where has = searchMenu
instance Has (Grid.Texts         Text) (Texts Text) where has = grid
instance Has (DropDownList.Texts Text) (Texts Text) where has = dropDownList
instance Has (TextEdit.Texts     Text) (Texts Text) where has = textEdit
instance Has (MainLoop.Texts     Text) (Texts Text) where has = mainLoop
instance Has (Zoom.Texts         Text) (Texts Text) where has = zoom
instance Has (StatusBar          Text) (Texts Text) where has = statusBar
instance Has (Collaboration      Text) (Texts Text) where has = collaborationTexts
instance Has (Name               Text) (Texts Text) where has = name
instance Has (Navigation         Text) (Texts Text) where has = navigationTexts
instance Has (Versioning         Text) (Texts Text) where has = versioning
instance Has (Code               Text) (Texts Text) where has = code
instance Has (CodeUI             Text) (Texts Text) where has = codeUI
instance Has (Sugars             Text) (Texts Text) where has = sugars
instance Has (Definitions        Text) (Texts Text) where has = definitions
instance Has (MomentuTexts.Texts Text) (Texts Text) where has = commonTexts
