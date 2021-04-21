{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}

module Test.Lamdu.Instances () where

import           Control.DeepSeq (NFData(..))
import           Control.Monad.Once (OnceT)
import           Control.Monad.Unit (Unit(..))
import qualified Data.ByteString.Char8 as BS8
import           Data.CurAndPrev (CurAndPrev)
import           Data.Data (Data)
import           Data.Property (Property(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Draw (Color(..))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
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
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.VersionControl.Config as VcGuiConfig
import qualified Lamdu.I18N.Fonts as I18N.Fonts
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import           Lamdu.Precedence (HasPrecedence(..))
import           Lamdu.Sugar.Annotations (ShowAnnotation)
import           Lamdu.Sugar.Internal (InternalName(..))
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
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

instance HasPrecedence InternalName where
    precedence (InternalName _ (T.Tag (Identifier ident)) _) =
        precedence (BS8.head ident)

instance NFData (Transaction m a) where rnf = pure () -- Cheating
instance NFData (OnceT (Transaction m) a) where rnf = pure () -- Cheating

instance Eq (Unit a) where _ == _ = True
instance Eq (a -> Unit b) where _ == _ = True
instance Eq a => Eq (Property Unit a) where
    Property x _ == Property y _ = x == y
instance Eq n => Eq (Sugar.TagPane n Unit) where
    Sugar.TagPane t0 n0 _ _ == Sugar.TagPane t1 n1 _ _ =
        t0 == t1 && n0 == n1

deriving instance (Eq a, Eq n) => Eq (Sugar.Annotation a n)
deriving instance (Eq a, Eq n) => Eq (Sugar.BinderParams a n Unit Unit)
deriving instance (Eq a, Eq n) => Eq (Sugar.CompositeFields n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.DefinitionOutdatedType n Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.FuncParam a n)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResBody n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResInject n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResRecord n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.ResTable n a)
deriving instance (Eq a, Eq n) => Eq (Sugar.Type n # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.AnnotatedArg v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.AssignPlain v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Assignment v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Binder v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Composite v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.CompositeItem v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.CompositeTail v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Definition v n Unit Unit a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.DefinitionBody v n Unit Unit a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.DefinitionExpression v n Unit Unit a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Else v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Fragment v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Function v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.IfElse v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.LabeledApply v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Lambda v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Let v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Nominal v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.OperatorArgs v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Pane v n Unit Unit a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.PaneBody v n Unit Unit a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.PostfixApply v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.PostfixFunc v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Repl v n Unit Unit a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.Term v n Unit Unit # Annotated a)
deriving instance (Eq a, Eq n, Eq v) => Eq (Sugar.WorkArea v n Unit Unit a)
deriving instance (Eq a, Eq n) => Eq (Sugar.PunnedVar n Unit # Annotated a)
deriving instance (Eq a, Eq n) => Eq (Sugar.NullaryInject n Unit Unit # Annotated a)
deriving instance Eq (Sugar.BinderVarInline Unit)
deriving instance Eq (Sugar.ClosedCompositeActions Unit)
deriving instance Eq (Sugar.Delete Unit)
deriving instance Eq (Sugar.DetachAction Unit)
deriving instance Eq (Sugar.EvalCompletionResult Unit)
deriving instance Eq (Sugar.EvalException Unit)
deriving instance Eq (Sugar.Hole n Unit Unit)
deriving instance Eq (Sugar.HoleOption n Unit Unit)
deriving instance Eq (Sugar.Literal (Property Unit))
deriving instance Eq (Sugar.NodeActions Unit)
deriving instance Eq (Sugar.NullParamActions Unit)
deriving instance Eq a => Eq (Sugar.ResList a)
deriving instance Eq a => Eq (Sugar.ResTree a)
deriving instance Eq n => Eq (Sugar.AddFirstParam n Unit Unit)
deriving instance Eq n => Eq (Sugar.AddNextParam n Unit Unit)
deriving instance Eq n => Eq (Sugar.BinderVarForm n Unit)
deriving instance Eq n => Eq (Sugar.BinderVarRef n Unit)
deriving instance Eq n => Eq (Sugar.DefinitionBuiltin n Unit)
deriving instance Eq n => Eq (Sugar.DefinitionForm n Unit)
deriving instance Eq n => Eq (Sugar.FuncParamActions n Unit Unit)
deriving instance Eq n => Eq (Sugar.GetVar n Unit)
deriving instance Eq n => Eq (Sugar.HoleResult n Unit Unit)
deriving instance Eq n => Eq (Sugar.Leaf n Unit Unit)
deriving instance Eq n => Eq (Sugar.NameRef n Unit)
deriving instance Eq n => Eq (Sugar.ParamInfo n Unit Unit)
deriving instance Eq n => Eq (Sugar.ParamRef n Unit)
deriving instance Eq n => Eq (Sugar.ResVal n)
deriving instance Eq n => Eq (Sugar.Scheme n)
deriving instance Eq n => Eq (Sugar.TagOption n Unit a)
deriving instance Eq n => Eq (Sugar.TagRef n Unit Unit)
deriving instance Eq n => Eq (Sugar.TagChoice n Unit Unit a)
deriving instance Eq v => Eq (Sugar.Payload v Unit)

instance (NFData a, NFData n) => NFData (Sugar.Annotation a n)
instance (NFData a, NFData n) => NFData (Sugar.BinderParams a n (OnceT (T i)) (T o))
instance (NFData a, NFData n) => NFData (Sugar.CompositeFields n a)
instance (NFData a, NFData n) => NFData (Sugar.DefinitionOutdatedType n (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.FuncParam a n)
instance (NFData a, NFData n) => NFData (Sugar.ResBody n a)
instance (NFData a, NFData n) => NFData (Sugar.ResInject n a)
instance (NFData a, NFData n) => NFData (Sugar.ResRecord n a)
instance (NFData a, NFData n) => NFData (Sugar.ResTable n a)
instance (NFData a, NFData n) => NFData (Sugar.Type n # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.AnnotatedArg v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.AssignPlain v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Assignment v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Binder v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Composite v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.CompositeItem v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.CompositeTail v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Definition v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.DefinitionBody v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.DefinitionExpression v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Else v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Fragment v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Function v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.IfElse v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.LabeledApply v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Lambda v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Let v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Nominal v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.OperatorArgs v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Pane v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.PaneBody v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.PostfixApply v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.PostfixFunc v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Repl v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.Term v n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n, NFData v) => NFData (Sugar.WorkArea v n (OnceT (T i)) (T o) a)
instance (NFData a, NFData n) => NFData (Sugar.NullaryInject n (OnceT (T i)) (T o) # Annotated a)
instance (NFData a, NFData n) => NFData (Sugar.PunnedVar n (T o) # Annotated a)
instance NFData (Sugar.AddFirstParam n (OnceT (T i)) (T o))
instance NFData (Sugar.AddNextParam n (OnceT (T i)) (T o))
instance NFData (Sugar.BinderVarInline (T o))
instance NFData (Sugar.ClosedCompositeActions (T o))
instance NFData (Sugar.Delete (T o))
instance NFData (Sugar.DetachAction (T o))
instance NFData (Sugar.EvalCompletionResult (T o))
instance NFData (Sugar.EvalException (T o))
instance NFData (Sugar.FuncParamActions n (OnceT (T i)) (T o))
instance NFData (Sugar.Hole n (OnceT (T i)) (T o))
instance NFData (Sugar.Literal (Property (T o)))
instance NFData (Sugar.NodeActions (T o))
instance NFData (Sugar.NullParamActions (T o))
instance NFData (Sugar.TagChoice n (OnceT (T i)) (T o) a)
instance NFData Def.FFIName
instance NFData EntityId
instance NFData ExprGui.GuiPayload
instance NFData LangId
instance NFData Name
instance NFData Name.Collision
instance NFData Name.TagName
instance NFData Name.TagText
instance NFData ShowAnnotation
instance NFData Sugar.BinderMode
instance NFData Sugar.BinderParamScopeId
instance NFData Sugar.CompiledErrorType
instance NFData Sugar.DefinitionState
instance NFData Sugar.Error
instance NFData Sugar.EvalTypeError
instance NFData Sugar.FuncApplyLimit
instance NFData Sugar.ParenInfo
instance NFData Sugar.ScopeId
instance NFData Sugar.VarInfo
instance NFData Tag.DirOp
instance NFData Tag.Symbol
instance NFData Tag.Tag
instance NFData Tag.TextsInLang
instance NFData a => NFData (CurAndPrev a)
instance NFData a => NFData (Property f a)
instance NFData a => NFData (Sugar.ResList a)
instance NFData a => NFData (Sugar.ResTree a)
instance NFData a => NFData (Sugar.SpecialArgs a)
instance NFData n => NFData (Sugar.BinderVarForm n (T o))
instance NFData n => NFData (Sugar.BinderVarRef n (T o))
instance NFData n => NFData (Sugar.DefinitionBuiltin n (T o))
instance NFData n => NFData (Sugar.DefinitionForm n (T o))
instance NFData n => NFData (Sugar.GetVar n (T o))
instance NFData n => NFData (Sugar.Leaf n (OnceT (T i)) (T o))
instance NFData n => NFData (Sugar.NameRef n (T o))
instance NFData n => NFData (Sugar.ParamInfo n (OnceT (T i)) (T o))
instance NFData n => NFData (Sugar.ParamRef n (T o))
instance NFData n => NFData (Sugar.ParamsRecordVarRef n)
instance NFData n => NFData (Sugar.ResVal n)
instance NFData n => NFData (Sugar.Scheme n)
instance NFData n => NFData (Sugar.TId n)
instance NFData n => NFData (Sugar.Tag n)
instance NFData n => NFData (Sugar.TagPane n (T o))
instance NFData n => NFData (Sugar.TagRef n (OnceT (T i)) (T o))
instance NFData v => NFData (Sugar.Payload v (T o))
