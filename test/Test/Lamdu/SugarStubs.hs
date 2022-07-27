-- | Build sugar expressions easily

module Test.Lamdu.SugarStubs where

import           Control.Monad.Unit (Unit(..))
import           Data.Property (Property(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import           Hyper.Syntax.Scheme (QVars(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Tag (TextsInLang(..), IsOperator(..))
import           Lamdu.Sugar.Internal (nameWithoutContext)
import           Lamdu.Sugar.Names.Add (InternalName(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Test.Lamdu.Prelude

prop :: a -> Property Unit a
prop x = Property x (const Unit)

type Expr =
    Sugar.Expr Sugar.Term (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName)
    InternalName Identity Unit

litNum :: Double -> Expr
litNum x = prop x & Sugar.LiteralNum & Sugar.LeafLiteral & Sugar.BodyLeaf & expr

defRef :: String -> T.Tag -> Sugar.GetVar InternalName Unit
defRef var tag =
    Sugar.GetVar
    { Sugar._vName = taggedEntityName (fromString var) tag
    , Sugar._vForm = Sugar.GetDefinition Sugar.DefUpToDate
    , Sugar._vGotoParam = Nothing
    , Sugar._vVar = fromString var
    , Sugar._vInline = Sugar.CannotInline
    }

node ::
    h # Annotated (Sugar.Payload (Sugar.Annotation v InternalName) Unit) ->
    Annotated (Sugar.Payload (Sugar.Annotation v InternalName) Unit) # h
node = Const payload & Ann

labeledApplyFunc ::
    Sugar.GetVar InternalName Unit ->
    Annotated (Sugar.Payload (Sugar.Annotation v InternalName) Unit) #
    Const (Sugar.GetVar InternalName Unit)
labeledApplyFunc = node . Const

type Infix2 = Expr -> Expr -> Expr

infix2Apply ::
    Sugar.GetVar InternalName Unit ->
    Infix2
infix2Apply varRef l r =
    Sugar.LabeledApply (labeledApplyFunc varRef) (Just (Sugar.OperatorArgs l r Unit)) [] []
    & Sugar.BodyLabeledApply
    & expr

arithmeticInfix2 :: String -> Infix2
arithmeticInfix2 op = infix2Apply (defRef (fromString op) (fromString op))

hole :: Expr
hole = Sugar.Hole mempty mempty & Sugar.LeafHole & Sugar.BodyLeaf & expr

($$) :: Expr -> Expr -> Expr
func $$ arg =
    V.App func arg
    & Sugar.BodySimpleApply
    & expr

($.) :: Expr -> T.Tag -> Expr
r $. tag =
    Sugar.PostfixApply
    { Sugar._pArg = r
    , Sugar._pFunc = mkTag Nothing tag & Sugar.PfGetField & node
    }
    & Sugar.BodyPostfixApply
    & expr

identity :: Expr
identity = defRef "id" "id" & Sugar.LeafGetVar & Sugar.BodyLeaf & expr

plus :: Infix2
plus = arithmeticInfix2 "+"

mul :: Infix2
mul = arithmeticInfix2 "*"

pane ::
    Sugar.Definition v name i Unit a ->
    Sugar.Pane v name i Unit a
pane body =
    Sugar.Pane
    { Sugar._paneBody = Sugar.PaneDefinition body
    , Sugar._paneEntityId = "dummy"
    , Sugar._paneDefinitionState = prop Sugar.LiveDefinition
    , Sugar._paneClose = Unit
    , Sugar._paneMoveDown = Nothing
    , Sugar._paneMoveUp = Nothing
    }

tagRefTag :: Maybe UUID -> T.Tag -> Sugar.Tag InternalName
tagRefTag var tag =
    Sugar.Tag
    { Sugar._tagName = maybe nameWithoutContext taggedEntityName var tag
    , Sugar._tagInstance = "dummy"
    , Sugar._tagVal = tag
    }

mkTag :: Maybe UUID -> T.Tag -> Sugar.TagRef InternalName Identity Unit
mkTag var tag =
    Sugar.TagRef
    { Sugar._tagRefReplace = Identity tagRefReplace
    , Sugar._tagRefTag = tagRefTag var tag
    , Sugar._tagRefJumpTo = Nothing
    }

mkOptionalTag :: Maybe UUID -> T.Tag -> Sugar.OptionalTag InternalName Identity Unit
mkOptionalTag var tag = Sugar.OptionalTag (mkTag var tag) Unit

def ::
    Annotated Sugar.EntityId # Sugar.Type InternalName Unit ->
    UUID -> T.Tag ->
    Annotated expr # Sugar.Assignment v InternalName Identity Unit ->
    Sugar.Definition v InternalName Identity Unit expr
def typ var tag body =
    Sugar.Definition
    { Sugar._drName = mkOptionalTag (Just var) tag
    , Sugar._drDefI = "def"
    , Sugar._drGotoNextOutdated = Unit
    , Sugar._drBody =
        Sugar.DefinitionBodyExpression Sugar.DefinitionExpression
        { Sugar._deType =
            Sugar.Scheme
            { Sugar._schemeForAll = emptyForalls
            , Sugar._schemeType = typ
            }
        , Sugar._dePresentationMode = Nothing
        , Sugar._deContent = body
        , Sugar._deVarInfo = Sugar.VarGeneric
        , Sugar._deResult = pure Nothing
        }
    }
    where
        emptyForalls = T.Types (QVars mempty) (QVars mempty)

funcExpr ::
    UUID -> T.Tag -> Expr ->
    Sugar.Body Sugar.Function (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName) InternalName Identity Unit
funcExpr paramVar paramTag (Ann (Const ba) bx) =
    Sugar.Function
    { Sugar._fChosenScopeProp = prop Nothing & pure
    , Sugar._fBodyScopes = mempty
    , Sugar._fParams =
        Sugar.LhsVar Sugar.Var
        { Sugar._vParam = Sugar.FuncParam Sugar.AnnotationNone [] Sugar.VarGeneric
        , Sugar._vTag = Sugar.OptionalTag (mkTag (Just paramVar) paramTag) Unit
        , Sugar._vAddPrev = Sugar.AddNext (Identity tagRefReplace)
        , Sugar._vAddNext = Sugar.AddNext (Identity tagRefReplace)
        , Sugar._vDelete = Unit
        , Sugar._vIsNullParam = False
        }
    , Sugar._fBody = Ann (Const ba) (Sugar.Binder Unit (Sugar.BinderTerm bx))
    }

expr ::
    Sugar.Body Sugar.Term (Sugar.Annotation v InternalName) InternalName Identity Unit ->
    Sugar.Expr Sugar.Term (Sugar.Annotation v InternalName) InternalName Identity Unit
expr = node

numType :: Annotated Sugar.EntityId # Sugar.Type InternalName Unit
numType =
    Sugar.TInst (Sugar.TId (taggedEntityName "numTid" "num") "num" Unit) mempty
    & Ann (Const "dummy")

payload :: (Sugar.Payload (Sugar.Annotation v InternalName) Unit)
payload =
    Sugar.Payload
    { Sugar._plAnnotation = Sugar.AnnotationNone
    , Sugar._plEntityId = "dummy"
    , Sugar._plHiddenEntityIds = []
    , Sugar._plActions = nodeActions
    , Sugar._plParenInfo = Sugar.ParenInfo 0 False
    }

nodeActions :: Sugar.NodeActions Unit
nodeActions =
    Sugar.NodeActions
    { Sugar._detach = Sugar.DetachAction Unit
    , Sugar._delete = Sugar.CannotDelete
    , Sugar._setToLiteral = pure Unit
    , Sugar._extract = Unit
    , Sugar._mReplaceParent = Nothing
    , Sugar._mApply = Nothing
    }

taggedEntityName :: UUID -> T.Tag -> InternalName
taggedEntityName ctx tag =
    InternalName
    { _inContext = Just ctx
    , _inTag = tag
    , _inIsAutoName = False
    }

tagRefReplace :: Sugar.TagChoice InternalName Unit
tagRefReplace =
    Sugar.TagChoice
    { Sugar._tcOptions = []
    , Sugar._tcNewTag =
        Sugar.TagOption
        { Sugar._toInfo = Sugar.Tag
            { Sugar._tagName = taggedEntityName "newTag" "newTag"
            , Sugar._tagInstance = "newTag"
            , Sugar._tagVal = "newTag"
            }
        , Sugar._toPick = Unit
        }
    }

getName :: T.Tag -> Identity (IsOperator, TextsInLang)
getName =
    pure . (\x -> (NotAnOperator, TextsInLang x Nothing Nothing)) .
    fromString . show
