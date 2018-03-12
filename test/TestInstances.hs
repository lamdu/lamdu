{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving, DeriveDataTypeable, OverloadedStrings #-}

module TestInstances () where

import           Data.Data (Data)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Draw (Color(..))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widgets.Menu as Menu
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.GUI.VersionControl.Config as VcGuiConfig
import           Text.PrettyPrint ((<+>))
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

deriving instance Data a => Data (Fonts a)
deriving instance Data a => Data (Vector2 a)
deriving instance Data Color
deriving instance Data Hover.Style
deriving instance Data Menu.Style
deriving instance Data ResponsiveExpr.Style
deriving instance Data Theme
deriving instance Data Theme.CodeForegroundColors
deriving instance Data Theme.Eval
deriving instance Data Theme.Help
deriving instance Data Theme.Hole
deriving instance Data Theme.Name
deriving instance Data Theme.ToolTip
deriving instance Data Theme.ValAnnotation
deriving instance Data VcGuiConfig.Theme

instance Pretty Color where
    pPrint (Color r g b a)
        | a == 1 = base
        | otherwise = base <+> pPrint a
        where
            base = "Color" <+> pPrint r <+> pPrint g <+> pPrint b
