-- | Language definitions
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module Lamdu.I18N.Language
    ( Language(..), lFonts, lTitle
    , HasLanguage
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config.Folder (HasConfigFolder(..))
import qualified Lamdu.Config.Folder as Folder
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Collaboration as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Fonts as I18N.Fonts
import           Lamdu.I18N.LangId (LangId)
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.I18N.Texts (Texts)
import qualified Lamdu.I18N.Versioning as Texts

import           Lamdu.Prelude

data Language = Language
    { _lDirection :: Dir.Layout
    , _lIdentifier :: LangId
    , _lFonts ::
        I18N.Fonts.ProportionalAndMonospace
        (I18N.Fonts.SansAndSerif
         (I18N.Fonts.RomanAndItalic
          (I18N.Fonts.LightAndBold FilePath)))
    , _lTitle :: Text
    , _lTexts :: Texts Text
    } deriving Eq

Lens.makeLenses ''Language
JsonTH.derivePrefixed "_l" ''Language

instance HasConfigFolder Language where
    type Folder Language = Folder.Language
    configFolderName _ = "languages"

type HasLanguage env =
    ( Has LangId env
    , Has Dir.Layout env
    , Grid.HasTexts env
    , SearchMenu.HasTexts env
    , TextEdit.HasTexts env
    , Has (Texts.Navigation    Text) env
    , Has (Texts.Versioning    Text) env
    , Has (Texts.CodeUI        Text) env
    , Has (Texts.Code          Text) env
    , Has (Texts.Collaboration Text) env
    , Has (Texts.Name          Text) env
    , Has (Texts.Definitions   Text) env
    , Has (Choice.Texts        Text) env
    , Has (Texts.Name          Text) env
    , Has (Map LangId          Text) env
    , Has Language env
    )

instance Has LangId Language where has = lIdentifier
instance Has Dir.Layout Language where has = lDirection
instance Has (f Text) (Texts Text) => Has (f Text) Language where
    has = lTexts . has
