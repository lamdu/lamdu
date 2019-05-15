-- | Language definitions
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module Lamdu.I18N.Language
    ( Language(..)
    , HasLanguage(..)
    , texts
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config.Folder (HasConfigFolder(..))
import           Lamdu.I18N.Texts
import           Lamdu.Name (NameTexts)
import Lamdu.I18N.LangId (LangId)

import           Lamdu.Prelude

data Language = Language
    { _lDirection :: Dir.Layout
    , _lIdentifier :: LangId
    , _lTexts :: Texts Text
    } deriving (Eq)

Lens.makeLenses ''Language
JsonTH.derivePrefixed "_l" ''Language

instance HasConfigFolder Language where
    configFolder _ = "languages"

class
    ( Glue.HasTexts env, Has (Dir.Texts Text) env, Has (Choice.Texts Text) env
    , TextEdit.HasTexts env, Grid.HasTexts env, Has (NameTexts Text) env
    , Has (Menu.Texts Text) env, SearchMenu.HasTexts env, Has LangId env
    , Has (Navigation Text) env
    ) => HasLanguage env where
    language :: Lens' env Language

instance Has LangId Language where has = lIdentifier
instance HasLanguage Language where language = id
instance Has Dir.Layout Language where has = lDirection
instance Has (f Text) (Texts Text) => Has (f Text) Language where
    has = lTexts . has

texts :: HasLanguage env => Lens' env (Texts Text)
texts = language . lTexts
