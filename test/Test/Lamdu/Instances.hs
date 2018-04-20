{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}

module Test.Lamdu.Instances () where

import qualified Data.ByteString.Char8 as BS8
import           Data.Data (Data)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import           GUI.Momentu.Animation (R)
import           GUI.Momentu.Draw (Color(..))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widgets.Menu as Menu
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Name as Theme
import           Lamdu.Config.Theme.TextColors as Theme
import           Lamdu.Config.Theme.ValAnnotation as Theme
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.GUI.VersionControl.Config as VcGuiConfig
import           Lamdu.Precedence (HasPrecedence(..))
import           Lamdu.Sugar.Internal (InternalName(..))
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import           Test.QuickCheck (Arbitrary(..), choose, getPositive, frequency)
import           Text.PrettyPrint ((<+>))
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

deriving instance Data Color
deriving instance Data Hover.Style
deriving instance Data Menu.Style
deriving instance Data ResponsiveExpr.Style
deriving instance Data Theme
deriving instance Data Theme.Eval
deriving instance Data Theme.Help
deriving instance Data Theme.Hole
deriving instance Data Theme.Name
deriving instance Data Theme.StatusBar
deriving instance Data Theme.TextColors
deriving instance Data Theme.ToolTip
deriving instance Data Theme.ValAnnotation
deriving instance Data VcGuiConfig.Theme
deriving instance Data a => Data (Fonts a)
deriving instance Data a => Data (Vector2 a)

instance IsString EntityId where
    fromString = EntityId . fromString

instance IsString UUID where
    fromString s =
        fromString (s ++ replicate (16 - length s) '\0')
        & UUID.fromByteString
        & fromMaybe (error ("Failed to convert to UUID: " <> show s))

instance Pretty Color where
    pPrint (Color r g b a)
        | a == 1 = base
        | otherwise = base <+> pPrint a
        where
            base = "Color" <+> pPrint r <+> pPrint g <+> pPrint b

instance Arbitrary (Vector2 R) where
    arbitrary =
        Vector2 <$> comp <*> comp
        where
            comp =
                frequency
                [ (1, pure 0)
                , (10, getPositive <$> arbitrary)
                ]

instance Arbitrary a => Arbitrary (Aligned a) where
    arbitrary =
        Aligned
        <$> (Vector2 <$> comp <*> comp)
        <*> arbitrary
        where
            comp =
                frequency
                [ (1, pure 0)
                , (1, pure 1)
                , (10, choose (0, 1))
                ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink (_ :| []) = []
    shrink (x0 :| (x1 : xs)) = (x1 :| xs) : (shrink (x1 : xs) <&> (x0 :|))

instance HasPrecedence InternalName where
    precedence (InternalName _ (T.Tag (Identifier ident))) =
        precedence (BS8.head ident)
