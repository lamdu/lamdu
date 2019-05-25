{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}

module Test.Lamdu.Instances () where

import           AST (Tree, Ann(..))
import           Control.DeepSeq (NFData(..))
import           Control.Monad.Unit (Unit(..))
import qualified Data.ByteString.Char8 as BS8
import           Data.CurAndPrev (CurAndPrev)
import           Data.Data (Data)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Property (Property(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import           GUI.Momentu.Animation (R)
import           GUI.Momentu.Draw (Color(..))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Generic.Random
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (Fonts(..))
import qualified Lamdu.Config.Theme.Name as Theme
import           Lamdu.Config.Theme.Sprites (Sprites(..))
import qualified Lamdu.Config.Theme.TextColors as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as Theme
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.VersionControl.Config as VcGuiConfig
import qualified Lamdu.I18N.Fonts as I18N.Fonts
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import           Lamdu.Precedence (HasPrecedence(..))
import           Lamdu.Sugar.Annotations (ShowAnnotation)
import           Lamdu.Sugar.Internal (InternalName(..))
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Test.QuickCheck (Arbitrary(..), choose, getPositive, frequency)
import           Text.PrettyPrint ((<+>))
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

type T = Transaction

deriving instance Data Color
deriving instance Data Hover.Style
deriving instance Data Menu.Style
deriving instance Data SearchMenu.TermStyle
deriving instance Data ResponsiveExpr.Style
deriving instance Data Theme
deriving instance Data I18N.Fonts.LightOrBold
deriving instance Data I18N.Fonts.RomanOrItalic
deriving instance Data I18N.Fonts.SansOrSerif
deriving instance Data I18N.Fonts.ProportionalOrMonospace
deriving instance Data Theme.FontSel
deriving instance Data Theme.Eval
deriving instance Data Theme.Help
deriving instance Data Theme.Hole
deriving instance Data Theme.Name
deriving instance Data Theme.StatusBar
deriving instance Data Theme.Deleted
deriving instance Data Theme.TextColors
deriving instance Data Theme.ToolTip
deriving instance Data Theme.ValAnnotation
deriving instance Data VcGuiConfig.Theme
deriving instance Data a => Data (TextEdit.Modes a)
deriving instance Data a => Data (Sprites a)
deriving instance Data a => Data (Fonts a)
deriving instance Data a => Data (Vector2 a)

instance IsString Widget.Id where
    fromString = Widget.Id . pure . fromString

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

instance Arbitrary Hover.Orientation where
    arbitrary = genericArbitrary uniform

instance Arbitrary Widget.Id where
    arbitrary = arbitrary <&> BS8.pack <&> (:[]) <&> Widget.Id

instance HasPrecedence InternalName where
    precedence (InternalName _ (T.Tag (Identifier ident))) =
        precedence (BS8.head ident)

instance NFData (Transaction m a) where rnf = pure () -- Cheating

instance Eq (Unit a) where _ == _ = True
instance Eq (a -> Unit b) where _ == _ = True
instance Eq a => Eq (Property Unit a) where
    Property x _ == Property y _ = x == y

deriving instance (Eq a, Eq n) => Eq (Sugar.AnnotatedArg n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Case n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Composite n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.CompositeFields n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.CompositeItem n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Definition n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionBody n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionExpression n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionOutdatedType n Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.FuncParam n Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.GetField n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Nominal n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.PaneBody n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Pane n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Payload n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Repl n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResBody n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResInject n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResRecord n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResTable n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.TBody n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.WorkArea n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.AssignPlain n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Assignment n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Binder n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Body n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Else n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.ElseIfContent n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Fragment n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Function n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.IfElse n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Inject n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.InjectContent n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.LabeledApply n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Lambda n Unit Unit) (Ann a))
deriving instance (Eq a, Eq n) => Eq (Tree (Sugar.Let n Unit Unit) (Ann a))
deriving instance Eq (Name Unit)
deriving instance Eq (Name.StoredName Unit)
deriving instance Eq (Sugar.BinderVarInline Unit)
deriving instance Eq (Sugar.ClosedCompositeActions Unit)
deriving instance Eq (Sugar.DetachAction Unit)
deriving instance Eq (Sugar.EvalException Unit)
deriving instance Eq (Sugar.Heal Unit)
deriving instance Eq (Sugar.Hole n Unit Unit)
deriving instance Eq (Sugar.HoleOption n Unit Unit)
deriving instance Eq (Sugar.Literal (Property Unit))
deriving instance Eq (Sugar.NullParamActions Unit)
deriving instance Eq (Sugar.OpenCompositeActions Unit)
deriving instance Eq a => Eq (Sugar.CaseArg Unit a)
deriving instance Eq a => Eq (Sugar.CaseKind Unit a)
deriving instance Eq a => Eq (Sugar.CompositeTail Unit a)
deriving instance Eq a => Eq (Sugar.ResList a)
deriving instance Eq a => Eq (Sugar.ResTree a)
deriving instance Eq n => Eq (Sugar.AddFirstParam n Unit Unit)
deriving instance Eq n => Eq (Sugar.AddNextParam n Unit Unit)
deriving instance Eq n => Eq (Sugar.Annotation n Unit)
deriving instance Eq n => Eq (Sugar.BinderParams n Unit Unit)
deriving instance Eq n => Eq (Sugar.BinderVarForm n Unit)
deriving instance Eq n => Eq (Sugar.BinderVarRef n Unit)
deriving instance Eq n => Eq (Sugar.DefinitionBuiltin n Unit)
deriving instance Eq n => Eq (Sugar.DefinitionForm n Unit)
deriving instance Eq n => Eq (Sugar.EvalCompletionResult n Unit)
deriving instance Eq n => Eq (Sugar.FuncParamActions n Unit Unit)
deriving instance Eq n => Eq (Sugar.GetVar n Unit)
deriving instance Eq n => Eq (Sugar.HoleResult n Unit Unit)
deriving instance Eq n => Eq (Sugar.NameRef n Unit)
deriving instance Eq n => Eq (Sugar.NodeActions n Unit Unit)
deriving instance Eq n => Eq (Sugar.NullaryVal n Unit Unit)
deriving instance Eq n => Eq (Sugar.ParamInfo n Unit Unit)
deriving instance Eq n => Eq (Sugar.ParamRef n Unit)
deriving instance Eq n => Eq (Sugar.ResVal n)
deriving instance Eq n => Eq (Sugar.Scheme n)
deriving instance Eq n => Eq (Sugar.TagRef n Unit Unit)
deriving instance Eq n => Eq (Sugar.TagOption n Unit a)
deriving instance Eq n => Eq (Sugar.TagReplace n Unit Unit a)
deriving instance Eq n => Eq (Sugar.Type n)
deriving instance Eq n => Eq (Sugar.ValAnnotation n Unit)

instance (NFData a, NFData n) => NFData (Sugar.AnnotatedArg n a)
instance (NFData a, NFData n) => NFData (Sugar.Case n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.Composite n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.CompositeFields n a)
instance (NFData a, NFData n) => NFData (Sugar.CompositeItem n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.Definition n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.DefinitionBody n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.DefinitionExpression n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.DefinitionOutdatedType n (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.FuncParam n (T i) a)
instance (NFData a, NFData n) => NFData (Sugar.GetField n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.Nominal n a)
instance (NFData a, NFData n) => NFData (Sugar.PaneBody n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.Pane n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.Payload n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.Repl n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.ResBody n a)
instance (NFData a, NFData n) => NFData (Sugar.ResInject n a)
instance (NFData a, NFData n) => NFData (Sugar.ResRecord n a)
instance (NFData a, NFData n) => NFData (Sugar.ResTable n a)
instance (NFData a, NFData n) => NFData (Sugar.TBody n a)
instance (NFData a, NFData n) => NFData (Sugar.WorkArea n (T i) (T o) a)
instance (NFData a, NFData n) => NFData (Tree (Sugar.AssignPlain n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Assignment n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Binder n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Body n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Else n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.ElseIfContent n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Fragment n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Function n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.IfElse n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Inject n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.InjectContent n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.LabeledApply n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Lambda n (T i) (T o)) (Ann a))
instance (NFData a, NFData n) => NFData (Tree (Sugar.Let n (T i) (T o)) (Ann a))
instance NFData (Name (T o))
instance NFData (Name.StoredName (T o))
instance NFData (Sugar.AddFirstParam n (T i) (T o))
instance NFData (Sugar.AddNextParam n (T i) (T o))
instance NFData (Sugar.BinderVarInline (T o))
instance NFData (Sugar.ClosedCompositeActions (T o))
instance NFData (Sugar.DetachAction (T o))
instance NFData (Sugar.EvalException (T o))
instance NFData (Sugar.FuncParamActions n (T i) (T o))
instance NFData (Sugar.Heal (Transaction o))
instance NFData (Sugar.Hole n (T i) (T o))
instance NFData (Sugar.Literal (Property (T o)))
instance NFData (Sugar.NodeActions n (T i) (T o))
instance NFData (Sugar.NullParamActions (T o))
instance NFData (Sugar.NullaryVal n (T i) (T o))
instance NFData (Sugar.OpenCompositeActions (T o))
instance NFData (Sugar.TagReplace n (T i) (T o) a)
instance NFData Def.FFIName
instance NFData EntityId
instance NFData ExprGui.Payload
instance NFData Name.Collision
instance NFData Name.TagText
instance NFData ShowAnnotation
instance NFData Sugar.BinderBodyScope
instance NFData Sugar.BinderMode
instance NFData Sugar.BinderParamScopeId
instance NFData Sugar.CompiledErrorType
instance NFData Sugar.Error
instance NFData Sugar.EvalTypeError
instance NFData Sugar.FuncApplyLimit
instance NFData Sugar.ScopeId
instance NFData Sugar.VarInfo
instance NFData a => NFData (CurAndPrev a)
instance NFData a => NFData (Property f a)
instance NFData a => NFData (Sugar.CaseArg (T o) a)
instance NFData a => NFData (Sugar.CaseKind (T o) a)
instance NFData a => NFData (Sugar.CompositeTail (T o) a)
instance NFData a => NFData (Sugar.ResList a)
instance NFData a => NFData (Sugar.ResTree a)
instance NFData a => NFData (Sugar.SpecialArgs a)
instance NFData n => NFData (Sugar.Annotation n (T i))
instance NFData n => NFData (Sugar.BinderParams n (T i) (T o))
instance NFData n => NFData (Sugar.BinderVarForm n (T o))
instance NFData n => NFData (Sugar.BinderVarRef n (T o))
instance NFData n => NFData (Sugar.DefinitionBuiltin n (T o))
instance NFData n => NFData (Sugar.DefinitionForm n (T o))
instance NFData n => NFData (Sugar.EvalCompletionResult n (T o))
instance NFData n => NFData (Sugar.GetVar n (T o))
instance NFData n => NFData (Sugar.NameRef n (T o))
instance NFData n => NFData (Sugar.ParamInfo n (T i) (T o))
instance NFData n => NFData (Sugar.ParamRef n (T o))
instance NFData n => NFData (Sugar.ParamsRecordVarRef n)
instance NFData n => NFData (Sugar.ResVal n)
instance NFData n => NFData (Sugar.Scheme n)
instance NFData n => NFData (Sugar.TId n)
instance NFData n => NFData (Sugar.TagRef n (T i) (T o))
instance NFData n => NFData (Sugar.Tag n)
instance NFData n => NFData (Sugar.Type n)
instance NFData n => NFData (Sugar.ValAnnotation n (T i))
