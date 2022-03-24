-- Work in progress

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Tests.Names (test) where

import           Control.Monad.Trans.FastWriter (Writer, runWriter)
import           Control.Monad.Unit (Unit(..))
import           Control.Monad.Writer (MonadWriter(..))
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import           Lamdu.Sugar.Names.Add (InternalName(..), addToWorkAreaTest)
import           Lamdu.Sugar.Names.CPS (liftCPS)
import qualified Lamdu.Sugar.Names.Walk as Walk
import qualified Lamdu.Sugar.Types as Sugar
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.SugarStubs ((~>))
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

newtype CollectNames name a = CollectNames { runCollectNames :: Writer [name] a }
    deriving newtype (Functor, Applicative, Monad, MonadWriter [name])

instance Walk.MonadNaming (CollectNames name) where
    type OldName (CollectNames name) = name
    type NewName (CollectNames name) = name
    type IM (CollectNames name) = Identity
    opGetName _ _ _ x = x <$ tell [x]
    opWithName _ _ x = x <$ liftCPS (tell [x])
    opRun = pure (pure . fst . runWriter . runCollectNames)
    opWithNewTag _ _ = id

test :: Test
test =
    testGroup "Disambiguation"
    [ testCase "disambiguation(#396)" workArea396
    , testCase "globals collide" workAreaGlobals
    , testCase "anonymous globals" anonGlobals
    ]

nameTexts :: Texts.Name Text
nameTexts =
    Texts.Name
    { Texts._unnamed = "Unnamed"
    , Texts._emptyName = "empty"
    }

assertNoCollisions :: Name -> IO ()
assertNoCollisions name =
    case Name.visible name nameTexts of
    (Name.TagText _ Name.NoCollision, Name.NoCollision) -> pure ()
    (Name.TagText text textCollision, tagCollision) ->
        unwords
        [ "Unexpected collision for name", show text
        , show textCollision, show tagCollision
        ] & assertString

testWorkArea ::
    (Name -> IO b) ->
    Sugar.WorkArea
        (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName)
        InternalName Identity Unit
        (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName) Unit) ->
    IO ()
testWorkArea verifyName inputWorkArea =
    do
        lang <- Env.makeLang
        addToWorkAreaTest lang Stub.getName inputWorkArea
            & runIdentity
            & getNames
            & traverse_ verifyName

getNames ::
    Sugar.WorkArea (Sugar.Annotation (Sugar.EvaluationScopes name Identity) name) name Identity o
        (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes name Identity) name) o) ->
    [name]
getNames workArea =
    Walk.toWorkAreaTest workArea
    & runCollectNames
    & runWriter
    & snd

--- test inputs:

workArea396 :: IO ()
workArea396 =
    Sugar.WorkArea
    { Sugar._waRepl = Stub.repl lamExpr
    , Sugar._waPanes =
        [ Stub.funcExpr "paneVar" "num" Stub.hole & Sugar.BodyFunction & Stub.node
            & Stub.def lamType "def" "def"
            & Stub.pane
        ]
    , Sugar._waGlobals = Sugar.Globals (pure []) (pure []) (pure [])
    } & testWorkArea assertNoCollisions
    where
        lamType = Stub.numType ~> Stub.numType
        lamExpr =
            Sugar.BodyLam Sugar.Lambda
            { Sugar._lamLightweight = False
            , Sugar._lamApplyLimit = Sugar.UnlimitedFuncApply
            , Sugar._lamFunc = Stub.funcExpr "lamVar" "num" Stub.hole
            } & Stub.expr

workAreaGlobals :: IO ()
workAreaGlobals =
    Sugar.WorkArea
    { Sugar._waRepl = Stub.repl Stub.hole
    , Sugar._waPanes =
        -- 2 defs sharing the same tag with different Vars/UUIDs,
        -- should collide with ordinary suffixes
        [ Stub.def Stub.numType "def1" "def" trivialBinder & Stub.pane
        , Stub.def Stub.numType "def2" "def" trivialBinder & Stub.pane
        ]
    , Sugar._waGlobals = Sugar.Globals (pure []) (pure []) (pure [])
    } & testWorkArea verifyName
    where
        verifyName name =
            case Name.visible name nameTexts of
            (Name.TagText _ Name.NoCollision, Name.NoCollision) -> pure ()
            (Name.TagText _ Name.NoCollision, Name.Collision _) -> pure ()
            (Name.TagText text textCollision, tagCollision) ->
                unwords
                [ "Unexpected/bad collision for name", show text
                , show textCollision, show tagCollision
                ] & assertString

trivialBinder ::
    Annotated (Sugar.Payload (Sugar.Annotation v InternalName) Unit) #
    Sugar.Assignment
        (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName)
        InternalName Identity Unit
trivialBinder =
    Sugar.Hole mempty mempty & Sugar.LeafHole & Sugar.BodyLeaf & Sugar.BinderTerm
    & Sugar.Binder Unit & Sugar.AssignPlain Unit
    & Sugar.BodyPlain
    & Ann (Const Stub.payload)

anonGlobals :: IO ()
anonGlobals =
    Sugar.WorkArea
    { Sugar._waRepl = Stub.repl Stub.hole
    , Sugar._waPanes =
        -- 2 defs sharing the same tag with different Vars/UUIDs,
        -- should collide with ordinary suffixes
        [ Stub.def Stub.numType "def1" anonTag trivialBinder & Stub.pane
        , Stub.def Stub.numType "def2" anonTag trivialBinder & Stub.pane
        ]
    , Sugar._waGlobals = Sugar.Globals (pure []) (pure []) (pure [])
    } & testWorkArea (\x -> length (show x) `seq` pure ())
