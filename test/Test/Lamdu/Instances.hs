{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}

module Test.Lamdu.Instances () where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Monad.Unit (Unit(..))
import qualified Data.ByteString.Char8 as BS8
import           Data.CurAndPrev (CurAndPrev)
import           Data.Data (Data)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Property (Property(..))
import           Data.String (IsString(..))
import           Data.Tree.Diverse (Ann(..))
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
import qualified Lamdu.Config.Theme.TextColors as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as Theme
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.VersionControl.Config as VcGuiConfig
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

instance NFData (Transaction m a) where
    rnf = pure () -- Cheating

instance Eq (Unit a) where _ == _ = True
instance Eq (a -> Unit b) where _ == _ = True
instance Eq a => Eq (Property Unit a) where
    Property x _ == Property y _ = x == y

deriving instance (Eq a, Eq n) => Eq (Sugar.AnnotatedArg n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.AssignmentBody n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.AssignPlain n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Binder n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Body n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Case n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Composite n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.CompositeItem n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Definition n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionBody n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionExpression n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionOutdatedType n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Else n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.ElseIfContent n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Fragment n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.FuncParam n Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Function n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.GetField n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.IfElse n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Inject n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.InjectContent n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.LabeledApply n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Lambda n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Let n Unit Unit (Ann a))
deriving instance (Eq a, Eq n) => Eq (Sugar.Nominal n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Pane n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Payload n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.RecordType n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Repl n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResBody n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResInject n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResRecord n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResTable n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.TBody n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.VariantType n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.WorkArea n Unit Unit a)
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
deriving instance Eq a => Eq (Sugar.ResStream a)
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
deriving instance Eq n => Eq (Sugar.Tag n Unit Unit)
deriving instance Eq n => Eq (Sugar.TagOption n Unit a)
deriving instance Eq n => Eq (Sugar.TagSelection n Unit Unit a)
deriving instance Eq n => Eq (Sugar.Type n)
deriving instance Eq n => Eq (Sugar.ValAnnotation n Unit)

instance (NFData a, NFData e) => NFData (Ann a e)
instance (NFData a, NFData n) => NFData (Sugar.AnnotatedArg n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.AssignmentBody n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.AssignPlain n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Binder n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Body n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Case n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Composite n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.CompositeFields tag n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.CompositeItem n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Definition n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.DefinitionBody n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.DefinitionExpression n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.DefinitionOutdatedType n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Else n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.ElseIfContent n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Fragment n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.FuncParam n (T i) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Function n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.GetField n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.IfElse n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Inject n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.InjectContent n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.LabeledApply n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Lambda n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Let n (T i) (T o) (Ann a)) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Nominal n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Pane n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Payload n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.Repl n (T i) (T o) a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.ResBody n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.ResInject n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.ResRecord n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.ResTable n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.TBody n a) where rnf = genericRnf
instance (NFData a, NFData n) => NFData (Sugar.WorkArea n (T i) (T o) a) where rnf = genericRnf
instance NFData (Name (T o)) where rnf = genericRnf
instance NFData (Name.StoredName (T o)) where rnf = genericRnf
instance NFData (Sugar.AddFirstParam n (T i) (T o)) where rnf = genericRnf
instance NFData (Sugar.AddNextParam n (T i) (T o)) where rnf = genericRnf
instance NFData (Sugar.BinderVarInline (T o)) where rnf = genericRnf
instance NFData (Sugar.ClosedCompositeActions (T o)) where rnf = genericRnf
instance NFData (Sugar.DetachAction (T o)) where rnf = genericRnf
instance NFData (Sugar.EvalException (T o)) where rnf = genericRnf
instance NFData (Sugar.FuncParamActions n (T i) (T o)) where rnf = genericRnf
instance NFData (Sugar.Heal (Transaction o)) where rnf = genericRnf
instance NFData (Sugar.Hole n (T i) (T o)) where rnf = genericRnf
instance NFData (Sugar.Literal (Property (T o))) where rnf = genericRnf
instance NFData (Sugar.NodeActions n (T i) (T o)) where rnf = genericRnf
instance NFData (Sugar.NullaryVal n (T i) (T o)) where rnf = genericRnf
instance NFData (Sugar.NullParamActions (T o)) where rnf = genericRnf
instance NFData (Sugar.OpenCompositeActions (T o)) where rnf = genericRnf
instance NFData (Sugar.TagSelection n (T i) (T o) a) where rnf = genericRnf
instance NFData a => NFData (CurAndPrev a) where rnf = genericRnf
instance NFData a => NFData (Property f a) where rnf = genericRnf
instance NFData a => NFData (Sugar.CaseArg (T o) a) where rnf = genericRnf
instance NFData a => NFData (Sugar.CaseKind (T o) a) where rnf = genericRnf
instance NFData a => NFData (Sugar.CompositeTail (T o) a) where rnf = genericRnf
instance NFData a => NFData (Sugar.ResStream a) where rnf = genericRnf
instance NFData a => NFData (Sugar.ResTree a) where rnf = genericRnf
instance NFData a => NFData (Sugar.SpecialArgs a) where rnf = genericRnf
instance NFData Def.FFIName where rnf = genericRnf
instance NFData EntityId where rnf = genericRnf
instance NFData ExprGui.Payload
instance NFData n => NFData (Sugar.Annotation n (T i)) where rnf = genericRnf
instance NFData n => NFData (Sugar.BinderParams n (T i) (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.BinderVarForm n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.BinderVarRef n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.DefinitionBuiltin n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.DefinitionForm n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.EvalCompletionResult n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.GetVar n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.NameRef n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.ParamInfo n (T i) (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.ParamRef n (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.ParamsRecordVarRef n) where rnf = genericRnf
instance NFData n => NFData (Sugar.ResVal n) where rnf = genericRnf
instance NFData n => NFData (Sugar.Scheme n) where rnf = genericRnf
instance NFData n => NFData (Sugar.Tag n (T i) (T o)) where rnf = genericRnf
instance NFData n => NFData (Sugar.TagInfo n) where rnf = genericRnf
instance NFData n => NFData (Sugar.TId n) where rnf = genericRnf
instance NFData n => NFData (Sugar.Type n) where rnf = genericRnf
instance NFData n => NFData (Sugar.ValAnnotation n (T i)) where rnf = genericRnf
instance NFData Name.Collision
instance NFData Name.TagText
instance NFData ShowAnnotation
instance NFData Sugar.BinderBodyScope where rnf = genericRnf
instance NFData Sugar.BinderMode where rnf = genericRnf
instance NFData Sugar.BinderParamScopeId where rnf = genericRnf
instance NFData Sugar.ErrorType where rnf = genericRnf
instance NFData Sugar.EvalTypeError
instance NFData Sugar.FuncApplyLimit
instance NFData Sugar.ScopeId
instance NFData Sugar.VarInfo
