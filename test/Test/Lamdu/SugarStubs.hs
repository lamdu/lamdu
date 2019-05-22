-- | Build sugar expressions easily

module Test.Lamdu.SugarStubs where

import           AST (Tree, Tie)
import           AST.Knot.Ann (Ann(..), val)
import           AST.Term.Scheme (QVars(..))
import           Control.Monad.Unit (Unit(Unit))
import           Data.CurAndPrev (CurAndPrev(CurAndPrev))
import           Data.Functor.Identity (Identity(..))
import           Data.Property (Property(..), MkProperty(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.I18N.Language (Language)
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Internal (nameWithoutContext)
import           Lamdu.Sugar.Names.Add (InternalName(..))
import qualified Lamdu.Sugar.Names.Add as AddNames
import qualified Lamdu.Sugar.Names.Walk as NameWalk
import qualified Lamdu.Sugar.Types as Sugar

import           Test.Lamdu.Prelude

infixr 1 ~>
(~>) :: Sugar.Type name -> Sugar.Type name -> Sugar.Type name
param ~> res = Sugar.TFun param res & Sugar.Type "dummy"

nameRef :: name -> Sugar.NameRef name Unit
nameRef = (`Sugar.NameRef` Unit)

prop :: a -> Property Unit a
prop x = Property x (const Unit)

type Expr =
    Sugar.Expression InternalName Identity Unit
    (Sugar.Payload InternalName Identity Unit ())

litNum :: Double -> Expr
litNum x = prop x & Sugar.LiteralNum & Sugar.BodyLiteral & expr

defRef :: String -> T.Tag -> Sugar.BinderVarRef InternalName Unit
defRef var tag =
    Sugar.BinderVarRef
    { Sugar._bvNameRef = nameRef (taggedEntityName (fromString var) tag)
    , Sugar._bvForm = Sugar.GetDefinition Sugar.DefUpToDate
    , Sugar._bvVar = fromString var
    , Sugar._bvInline = Sugar.CannotInline
    }

node ::
    Tie knot (Ann (Sugar.Payload name Identity Unit ())) ->
    Ann (Sugar.Payload name Identity Unit ()) knot
node = Ann payload

labeledApplyFunc ::
    Sugar.BinderVarRef name Unit ->
    Tree (Ann (Sugar.Payload name Identity Unit ()))
    (Const (Sugar.BinderVarRef name Unit))
labeledApplyFunc = node . Const

type Infix2 = Expr -> Expr -> Expr

infix2Apply ::
    Sugar.BinderVarRef InternalName Unit ->
    Infix2
infix2Apply varRef l r =
    Sugar.LabeledApply (labeledApplyFunc varRef) (Sugar.Infix l r) [] []
    & Sugar.BodyLabeledApply
    & expr

arithmeticInfix2 :: String -> Infix2
arithmeticInfix2 op = infix2Apply (defRef (fromString op) (fromString op))

hole :: Expr
hole =
    Sugar.BodyHole Sugar.Hole
    { Sugar._holeOptions = mempty
    , Sugar._holeOptionLiteral = error "TODO: option literal"
    , Sugar._holeMDelete = Nothing
    } & expr

($$) :: Expr -> Expr -> Expr
func $$ arg =
    V.Apply func arg
    & Sugar.BodySimpleApply
    & expr

($.) :: Expr -> T.Tag -> Expr
r $. tag =
    Sugar.GetField
    { Sugar._gfRecord = r
    , Sugar._gfTag = mkTag Nothing tag
    }
    & Sugar.BodyGetField
    & expr

identity :: Expr
identity =
    defRef "id" "id"
    & Sugar.GetBinder
    & Sugar.BodyGetVar
    & expr

plus :: Infix2
plus = arithmeticInfix2 "+"

mul :: Infix2
mul = arithmeticInfix2 "*"

pane :: Sugar.Definition name i Unit a -> Sugar.Pane name i Unit a
pane body =
    Sugar.Pane
    { Sugar._paneDefinition = body
    , Sugar._paneClose = Unit
    , Sugar._paneMoveDown = Nothing
    , Sugar._paneMoveUp = Nothing
    }

tagInfo :: Maybe UUID -> T.Tag -> Sugar.TagInfo InternalName
tagInfo var tag =
    Sugar.TagInfo
    { Sugar._tagName = maybe nameWithoutContext taggedEntityName var tag
    , Sugar._tagInstance = "dummy"
    , Sugar._tagVal = tag
    }

mkTag :: Maybe UUID -> T.Tag -> Sugar.Tag InternalName Identity Unit
mkTag var tag =
    Sugar.Tag
    { Sugar._tagSelection = tagSelection
    , Sugar._tagInfo = tagInfo var tag
    }

def ::
    Sugar.Type InternalName -> UUID -> T.Tag ->
    Tree (Ann expr) (Sugar.Assignment InternalName Identity Unit) ->
    Sugar.Definition InternalName Identity Unit expr
def typ var tag body =
    Sugar.Definition
    { Sugar._drName = mkTag (Just var) tag
    , Sugar._drDefI = "def"
    , Sugar._drDefinitionState = prop Sugar.LiveDefinition & pure
    , Sugar._drEntityId = "dummy"
    , Sugar._drBody =
        Sugar.DefinitionBodyExpression Sugar.DefinitionExpression
        { Sugar._deType =
            Sugar.Scheme
            { Sugar._schemeForAll = emptyForalls
            , Sugar._schemeType = typ
            }
        , Sugar._dePresentationMode = Nothing
        , Sugar._deContent = body
        }
    }
    where
        emptyForalls = T.Types (QVars mempty) (QVars mempty)

repl :: Sugar.Expression name i o a -> Sugar.Repl name i o a
repl (Ann pl x) =
    Sugar.Repl
    { Sugar._replExpr = Ann pl (Sugar.BinderExpr x)
    , Sugar._replVarInfo = Sugar.VarNormal
    , Sugar._replResult = CurAndPrev Nothing Nothing
    }

mkFuncParam ::
    (UUID, T.Tag) ->
    Sugar.FuncParam name Identity (Sugar.ParamInfo InternalName Identity Unit)
mkFuncParam (paramVar, paramTag) =
    Sugar.FuncParam
    { Sugar._fpAnnotation = Sugar.AnnotationNone
    , Sugar._fpInfo =
        Sugar.ParamInfo
        { Sugar._piTag = mkTag (Just paramVar) paramTag
        , Sugar._piActions =
            Sugar.FuncParamActions
            { Sugar._fpAddNext = Sugar.AddNext tagSelection
            , Sugar._fpDelete = Unit
            , Sugar._fpMOrderBefore = Nothing
            , Sugar._fpMOrderAfter = Nothing
            }
        }
    , Sugar._fpVarInfo = Sugar.VarNormal
    }

funcExpr ::
    [(UUID, T.Tag)] -> Expr ->
    Tree (Sugar.Function InternalName Identity Unit)
    (Ann (Sugar.Payload InternalName Identity Unit ()))
funcExpr params pn =
    Sugar.Function
    { Sugar._fChosenScopeProp = prop Nothing & pure
    , Sugar._fBodyScopes = CurAndPrev mempty mempty & Sugar.BinderBodyScope
    , Sugar._fAddFirstParam = Sugar.PrependParam tagSelection
    , Sugar._fParams = params <&> mkFuncParam & Sugar.Params
    , Sugar._fBody = pn & val %~ Sugar.BinderExpr
    }

binderExpr ::
    [(UUID, T.Tag)] -> Expr ->
    Tree (Ann (Sugar.Payload InternalName Identity Unit ()))
    (Sugar.Assignment InternalName Identity Unit)
binderExpr params body = funcExpr params body & Sugar.BodyFunction & node

expr ::
    Tree (Sugar.Body name Identity Unit) (Ann (Sugar.Payload name Identity Unit ())) ->
    Sugar.Expression name Identity Unit (Sugar.Payload name Identity Unit ())
expr = node

numType :: Sugar.Type InternalName
numType =
    Sugar.Type
    { Sugar._tPayload = "dummy"
    , Sugar._tBody = Sugar.TInst (Sugar.TId (taggedEntityName "numTid" "num") "num") mempty
    }

payload :: Sugar.Payload name Identity Unit ()
payload =
    Sugar.Payload
    { Sugar._plAnnotation = Sugar.AnnotationNone
    , Sugar._plNeverShrinkAnnotation = False
    , Sugar._plEntityId = "dummy"
    , Sugar._plActions = nodeActions
    , Sugar._plData = ()
    }

nodeActions :: Sugar.NodeActions name Identity Unit
nodeActions =
    Sugar.NodeActions
    { Sugar._detach = Sugar.DetachAction Unit
    , Sugar._mSetToHole = Nothing
    , Sugar._setToLiteral = pure Unit
    , Sugar._extract = Unit
    , Sugar._mReplaceParent = Nothing
    , Sugar._wrapInRecord = tagSelection
    , Sugar._mNewLet = Nothing
    }

taggedEntityName :: UUID -> T.Tag -> InternalName
taggedEntityName ctx tag =
    InternalName
    { _inContext = Just ctx
    , _inTag = tag
    }

tagSelection :: Sugar.TagSelection name Identity Unit ()
tagSelection =
    Sugar.TagSelection
    { Sugar._tsOptions = pure []
    , Sugar._tsNewTag = const Unit
    , Sugar._tsAnon = Nothing
    }

addNamesToExpr ::
    Language ->
    Sugar.Expression InternalName Identity Unit
    (Sugar.Payload InternalName Identity Unit a) ->
    Sugar.Expression (Name Unit) Identity Unit
    (Sugar.Payload (Name Unit) Identity Unit a)
addNamesToExpr lang x =
    AddNames.runPasses lang
    getNameProp NameWalk.toExpression NameWalk.toExpression NameWalk.toExpression x
    & runIdentity

getNameProp :: T.Tag -> MkProperty Identity Unit Text
getNameProp tag =
    Property (fromString (show tag)) (const Unit)
    & Identity & MkProperty
