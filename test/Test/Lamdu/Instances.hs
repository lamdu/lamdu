{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, UndecidableInstances, TemplateHaskell #-}

module Test.Lamdu.Instances () where

import           Control.DeepSeq (NFData(..))
import           Control.Monad.Once (OnceT)
import qualified Data.ByteString.Char8 as BS8
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Data (Data)
import           Data.Property (Property(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Data.Vector.Vector2 (Vector2(..))
import           Generics.Constraints (makeDerivings, makeInstances)
import           GUI.Momentu (Color(..))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Zoom as Zoom
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (Fonts(..))
import qualified Lamdu.Config.Theme.Name as Theme
import           Lamdu.Config.Theme.Sprites (Sprites(..))
import qualified Lamdu.Config.Theme.TextColors as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as Theme
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug.Tasks as DebugTasks
import qualified Lamdu.GUI.VersionControl.Config as VcGuiConfig
import qualified Lamdu.I18N.Fonts as I18N.Fonts
import           Lamdu.I18N.LangId (LangId(..))
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import           Lamdu.Precedence (HasPrecedence(..))
import           Lamdu.Sugar.Annotations (ShowAnnotation(..))
import qualified Lamdu.Sugar.Config as SugarConfig
import           Lamdu.Sugar.Internal (InternalName(..))
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Text.PrettyPrint ((<+>))
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

makeDerivings [''Data]
    [ ''Color, ''Hover.Style, ''Menu.Style, ''SearchMenu.TermStyle, ''ResponsiveExpr.Style
    , ''I18N.Fonts.LightOrBold, ''I18N.Fonts.RomanOrItalic, ''I18N.Fonts.SansOrSerif
    , ''I18N.Fonts.ProportionalOrMonospace
    , ''Theme, ''Theme.FontSel, ''Theme.Eval, ''Theme.Help, ''Theme.Name, ''Theme.StatusBar
    , ''Theme.Deleted, ''Theme.TextColors, ''Theme.ToolTip, ''Theme.ValAnnotation
    , ''VcGuiConfig.Theme, ''TextEdit.Modes, ''Sprites, ''Fonts, ''Vector2
    ]

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

instance NFData (OnceT (Transaction m) a) where rnf = pure () -- Cheating
instance NFData (Transaction m a) where rnf = pure () -- Cheating
instance NFData a => NFData (Property m a) where rnf (Property x _) = rnf x -- Cheating

instance Eq (a -> Proxy b) where _ == _ = True
instance Eq a => Eq (Property Proxy a) where Property x _ == Property y _ = x == y
instance Eq (Sugar.TagPane f) where
    x == y = x ^. Sugar.tpTag == y ^. Sugar.tpTag

[makeDerivings [''Eq], makeInstances [''NFData]] ??
    [ ''Sugar.Annotation, ''Sugar.LhsField, ''Sugar.LhsNames, ''Sugar.CompositeFields
    , ''Sugar.DefinitionOutdatedType, ''Sugar.FuncParam, ''Sugar.Type
    , ''Sugar.ResInject, ''Sugar.ResTable, ''Sugar.ResTree, ''Sugar.Result
    , ''Sugar.AnnotatedArg, ''Sugar.AssignPlain, ''Sugar.Assignment, ''Sugar.Binder, ''Sugar.BinderBody
    , ''Sugar.Composite, ''Sugar.CompositeTail, ''Sugar.OptionalTag
    , ''Sugar.TaggedItem, ''Sugar.TaggedSwappableItem, ''Sugar.TaggedList, ''Sugar.TaggedListBody
    , ''Sugar.Definition , ''Sugar.DefinitionBody, ''Sugar.DefinitionExpression
    , ''Sugar.Else, ''Sugar.ElseIfBody, ''Sugar.Fragment, ''Sugar.Function, ''Sugar.IfElse
    , ''Sugar.LabeledApply, ''Sugar.Lambda, ''Sugar.Let
    , ''Sugar.Nominal, ''Sugar.NullaryInject, ''Sugar.OperatorArgs, ''Sugar.Pane, ''Sugar.PaneBody
    , ''Sugar.PostfixApply, ''Sugar.PostfixFunc, ''Sugar.PunnedVar, ''Sugar.Term
    , ''Sugar.WorkArea, ''Sugar.VarInline, ''Sugar.ClosedCompositeActions, ''Sugar.Delete
    , ''Sugar.DetachAction, ''Sugar.EvalCompletionResult, ''Sugar.EvalException, ''Sugar.Hole
    , ''Sugar.Literal, ''Sugar.NodeActions, ''Sugar.Globals
    , ''Sugar.AddParam, ''Sugar.VarForm, ''Sugar.Payload
    , ''Sugar.DefinitionBuiltin, ''Sugar.DefinitionForm, ''Sugar.NominalPane
    , ''Sugar.GetVar, ''Sugar.Leaf, ''Sugar.NameRef, ''Sugar.Var
    , ''Sugar.Scheme, ''Sugar.TagRef, ''Sugar.TagChoice, ''Sugar.TagOption
    ] & sequenceA <&> concat

makeInstances [''NFData]
    [ ''DebugTasks.Tasks, ''SugarConfig.Sugars
    , ''Config, ''Config.Completion, ''Config.Debug, ''Config.Eval
    , ''Config.Export, ''Config.Literal, ''Config.Pane
    , ''Grid.Keys, ''Menu.Config, ''SearchMenu.Config
    , ''StdKeys.DirKeys, ''TextEdit.Keys, ''VcGuiConfig.Config, ''Zoom.Config
    , ''Sugar.TId, ''Sugar.Tag, ''Sugar.TagPane
    , ''Sugar.ScopeId, ''Sugar.DefinitionState, ''Sugar.ParenInfo, ''Sugar.VarInfo
    , ''Sugar.BinderParamScopeId, ''Sugar.FuncApplyLimit, ''Sugar.Error
    , ''Sugar.CompiledErrorType, ''ShowAnnotation, ''LangId, ''EntityId, ''Sugar.ParamKind
    , ''Name, ''Name.Collision, ''Name.TagName, ''Name.TagText
    , ''Tag.TextsInLang, ''Def.FFIName, ''Tag.DirOp, ''Tag.Symbol, ''Tag.Tag, ''CurAndPrev
    , ''Sugar.TaggedVarId
    ]
