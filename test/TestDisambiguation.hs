-- Work in progress

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module TestDisambiguation (test) where

import           Control.Applicative (Const(..))
import           Control.Monad.Trans.FastWriter (Writer, runWriter)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Property (Property(..), MkProperty(..))
import           Data.String (IsString(..))
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import           Lamdu.Sugar.Names.Add (InternalName(..), addToWorkArea)
import           Lamdu.Sugar.Names.CPS (liftCPS)
import qualified Lamdu.Sugar.Names.Walk as Walk
import qualified Lamdu.Sugar.Types as Sugar
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertString)
import           Test.Lamdu.Instances ()

import           Lamdu.Prelude

type Unit = Const ()

newtype CollectNames name a = CollectNames { runCollectNames :: Writer [name] a }
    deriving (Functor, Applicative, Monad, MonadWriter [name])

instance Walk.MonadNaming (CollectNames name) where
    type OldName (CollectNames name) = name
    type NewName (CollectNames name) = name
    type IM (CollectNames name) = Identity
    opGetName _ _ x = x <$ tell [x]
    opWithName _ _ x = x <$ liftCPS (tell [x])
    opRun = pure (pure . fst . runWriter . runCollectNames)

test :: Test
test =
    testGroup "Disambiguation"
    [ testCase "disambiguation(#396)" workArea396
    , testCase "globals collide" workAreaGlobals
    ]

assertNoCollisions :: Name o -> IO ()
assertNoCollisions name =
    case Name.visible name of
    (Name.TagText _ Name.NoCollision, Name.NoCollision) -> pure ()
    (Name.TagText text textCollision, tagCollision) ->
        unwords
        [ "Unexpected collision for name", show text
        , show textCollision, show tagCollision
        ] & assertString

testWorkArea ::
    (Name Unit -> IO b) -> Sugar.WorkArea InternalName Identity Unit a -> IO ()
testWorkArea verifyName inputWorkArea =
    addToWorkArea getNameProp inputWorkArea
    & runIdentity
    & getNames
    & traverse_ verifyName

getNames :: Sugar.WorkArea name Identity o a -> [name]
getNames workArea =
    Walk.toWorkArea workArea
    & runCollectNames
    & runWriter
    & snd

getNameProp :: T.Tag -> MkProperty Identity Unit Text
getNameProp tag =
    Property (fromString (show tag)) (const (Const ()))
    & Identity & MkProperty

pane ::
    Sugar.Definition name i (Const ()) (Sugar.Expression name i (Const ()) a) ->
    Sugar.Pane name i (Const ()) a
pane body =
    Sugar.Pane
    { Sugar._paneDefinition = body
    , Sugar._paneClose = Const ()
    , Sugar._paneMoveDown = Nothing
    , Sugar._paneMoveUp = Nothing
    }

tagInfo :: UUID -> T.Tag -> Sugar.TagInfo InternalName
tagInfo var tag =
    Sugar.TagInfo
    { Sugar._tagName = taggedEntityName var tag
    , Sugar._tagInstance = "dummy"
    , Sugar._tagVal = tag
    }

mkTag :: UUID -> T.Tag -> Sugar.Tag InternalName Identity Unit
mkTag var tag =
    Sugar.Tag
    { Sugar._tagSelection = tagSelection
    , Sugar._tagInfo = tagInfo var tag
    }

def ::
    Sugar.Type InternalName -> UUID -> T.Tag ->
    Sugar.Binder InternalName Identity Unit expr ->
    Sugar.Definition InternalName Identity Unit expr
def typ var tag body =
    Sugar.Definition
    { Sugar._drName = mkTag var tag
    , Sugar._drDefI = "def"
    , Sugar._drDefinitionState = Property Sugar.LiveDefinition (const (Const ())) & Identity
    , Sugar._drEntityId = "dummy"
    , Sugar._drBody =
        Sugar.DefinitionBodyExpression Sugar.DefinitionExpression
        { Sugar._deType =
            Sugar.Scheme
            { Sugar._schemeForAll = mempty
            , Sugar._schemeConstraints = mempty
            , Sugar._schemeType = typ
            }
        , Sugar._dePresentationMode = Nothing
        , Sugar._deContent = body
        }
    }

repl :: Sugar.Expression name i o a -> Sugar.Repl name i o a
repl x =
    Sugar.Repl
    { Sugar._replExpr = x
    , Sugar._replResult = CurAndPrev Nothing Nothing
    }

mkFuncParam ::
    (UUID, T.Tag, Sugar.Type name) ->
    Sugar.FuncParam name (Sugar.ParamInfo InternalName Identity Unit)
mkFuncParam (paramVar, paramTag, paramType) =
    Sugar.FuncParam
    { Sugar._fpAnnotation = annotation paramType
    , Sugar._fpInfo =
        Sugar.ParamInfo
        { Sugar._piTag = mkTag paramVar paramTag
        , Sugar._piActions =
            Sugar.FuncParamActions
            { Sugar._fpAddNext = Sugar.AddNext tagSelection
            , Sugar._fpDelete = Const ()
            , Sugar._fpMOrderBefore = Nothing
            , Sugar._fpMOrderAfter = Nothing
            }
        }
    }

binderExpr ::
    [(UUID, T.Tag, Sugar.Type InternalName)] ->
    Sugar.Expression InternalName Identity Unit () ->
    Sugar.Binder InternalName Identity Unit
    (Sugar.Expression InternalName Identity Unit ())
binderExpr params body =
    Sugar.Binder
    { Sugar._bChosenScopeProp = Property Nothing (const (Const ())) & Identity
    , Sugar._bLamId =
        case params of
        [] -> Nothing
        _:_ -> Just "dummy"
    , Sugar._bBodyScopes = CurAndPrev mempty mempty & Sugar.BinderBodyScope
    , Sugar._bActions =
        Sugar.BinderActions
        { Sugar._baAddFirstParam = Sugar.PrependParam tagSelection
        , Sugar._baMNodeActions = Just nodeActions
        }
    , Sugar._bParams = params <&> mkFuncParam & Sugar.Params
    , Sugar._bBody =
        Sugar.BinderBody
        { Sugar._bbAddOuterLet = Const ()
        , Sugar._bbContent = Sugar.BinderExpr body
        }
    }

funcType :: Sugar.Type InternalName -> Sugar.Type InternalName -> Sugar.Type InternalName
funcType param res =
    Sugar.Type
    { Sugar._tPayload = "dummy"
    , Sugar._tBody = Sugar.TFun param res
    }

expr ::
    Sugar.Type name ->
    Sugar.Body name Identity Unit
    (Sugar.Expression name Identity Unit ()) ->
    Sugar.Expression name Identity Unit ()
expr typ body =
    Sugar.Expression { Sugar._rBody = body, Sugar._rPayload = mkPayload typ }

numType :: Sugar.Type InternalName
numType =
    Sugar.Type
    { Sugar._tPayload = "dummy"
    , Sugar._tBody = Sugar.TInst (Sugar.TId (taggedEntityName "numTid" "num") "num") mempty
    }

mkPayload :: Sugar.Type name -> Sugar.Payload name Identity Unit ()
mkPayload typ =
    Sugar.Payload
    { Sugar._plAnnotation = annotation typ
    , Sugar._plEntityId = "dummy"
    , Sugar._plActions = nodeActions
    , Sugar._plData = ()
    }

annotation :: Sugar.Type name -> Sugar.Annotation name
annotation typ =
    Sugar.Annotation
    { Sugar._aInferredType = typ
    , Sugar._aMEvaluationResult = CurAndPrev Nothing Nothing
    }

nodeActions :: Sugar.NodeActions name Identity Unit
nodeActions =
    Sugar.NodeActions
    { Sugar._detach = Sugar.DetachAction (Const ())
    , Sugar._mSetToHole = Nothing
    , Sugar._extract = Const ()
    , Sugar._mReplaceParent = Nothing
    , Sugar._wrapInRecord = tagSelection
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
    { Sugar._tsOptions = Identity []
    , Sugar._tsNewTag = const (Const ())
    , Sugar._tsAnon = Nothing
    }


--- test inputs:

workArea396 :: IO ()
workArea396 =
    Sugar.WorkArea
    { Sugar._waRepl = repl lamExpr
    , Sugar._waPanes =
        [ binderExpr [("paneVar", "num", numType)] leafExpr
            & def lamType "def" "def"
            & pane
        ]
    } & testWorkArea assertNoCollisions
    where
        lamType = funcType numType numType
        leafExpr = expr numType Sugar.BodyPlaceHolder
        lamExpr =
            Sugar.BodyLam Sugar.Lambda
            { Sugar._lamMode = Sugar.NormalBinder
            , Sugar._lamBinder = binderExpr [("lamVar", "num", numType)] leafExpr
            } & expr lamType

workAreaGlobals :: IO ()
workAreaGlobals =
    Sugar.WorkArea
    { Sugar._waRepl = repl trivialExpr
    , Sugar._waPanes =
        -- 2 defs sharing the same tag with different Vars/UUIDs,
        -- should collide with ordinary suffixes
        [ def numType "def1" "def" trivialBinder & pane
        , def numType "def2" "def" trivialBinder & pane
        ]
    } & testWorkArea verifyName
    where
        verifyName name =
            case Name.visible name of
            (Name.TagText _ Name.NoCollision, Name.NoCollision) -> pure ()
            (Name.TagText _ Name.NoCollision, Name.Collision _) -> pure ()
            (Name.TagText text textCollision, tagCollision) ->
                unwords
                [ "Unexpected/bad collision for name", show text
                , show textCollision, show tagCollision
                ] & assertString
        trivialBinder = binderExpr [] trivialExpr
        trivialExpr = expr numType Sugar.BodyPlaceHolder
