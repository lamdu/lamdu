-- | Language definitions
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module Lamdu.I18N.Language
    ( Language(..), lFonts, lTitle, lCasing
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Direction as Dir
import           Lamdu.Config.Folder (HasConfigFolder(..))
import qualified Lamdu.Config.Folder as Folder
import qualified Lamdu.I18N.Fonts as I18N.Fonts
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Texts (Texts)

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
    , -- Whether the language supports capitalization (i.e has upper-case and lower-case)
      _lCasing :: Bool
    } deriving Eq

Lens.makeLenses ''Language
JsonTH.derivePrefixed "_l" ''Language

instance HasConfigFolder Language where
    type Folder Language = Folder.Language
    configFolderName _ = "languages"

instance Has LangId Language where has = lIdentifier
instance Has Dir.Layout Language where has = lDirection
instance Has (f Text) (Texts Text) => Has (f Text) Language where
    has = lTexts . has
