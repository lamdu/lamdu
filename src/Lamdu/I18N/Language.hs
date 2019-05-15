-- | Language definitions
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module Lamdu.I18N.Language
    ( Language(..)
    , HasLanguage
    , texts
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config.Folder (HasConfigFolder(..))
import           Lamdu.I18N.LangId (LangId)
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.I18N.Texts

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

type HasLanguage env =
    ( Has LangId env
    , Has Dir.Layout env
    , Has (Glue.Texts       Text) env
    , Has (Dir.Texts        Text) env
    , Has (Choice.Texts     Text) env
    , Has (Texts.Name       Text) env
    , Has (Menu.Texts       Text) env
    , Has (Navigation       Text) env
    , Has (Versioning       Text) env
    , Has (CodeUI           Text) env
    , Has (TextEdit.Texts   Text) env
    , Has (Grid.Texts       Text) env
    , Has (SearchMenu.Texts Text) env
    , Has (Definitions      Text) env
    , Has (MomentuTexts.Texts Text) env
    , Has Language env
    )

instance Has LangId Language where has = lIdentifier
instance Has Dir.Layout Language where has = lDirection
instance Has (f Text) (Texts Text) => Has (f Text) Language where
    has = lTexts . has

texts :: Has Language env => Lens' env (Texts Text)
texts = has . lTexts
