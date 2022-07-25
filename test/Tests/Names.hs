-- Work in progress

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Tests.Names (test) where

import           Control.Monad.Trans.FastWriter (Writer, runWriter)
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
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

newtype CollectNames name a = CollectNames { runCollectNames :: Writer [name] a }
    deriving newtype (Functor, Applicative, Monad, MonadWriter [name])

instance Walk.MonadNameWalk (CollectNames name) where
    type OldName (CollectNames name) = name
    type NewName (CollectNames name) = name
    type IM (CollectNames name) = Identity
    opGetName _ _ _ x = x <$ tell [x]
    opWithName _ _ x = x <$ liftCPS (tell [x])
    opRun = pure (pure . fst . runWriter . runCollectNames)
    opWithNewTag _ _ = id

test :: TestTree
test =
    testGroup "Disambiguation"
    [ testCase "globals collide" workAreaGlobals
    , testCase "anonymous globals" anonGlobals
    ]

nameTexts :: Texts.Name Text
nameTexts =
    Texts.Name
    { Texts._unnamed = "Unnamed"
    , Texts._emptyName = "empty"
    }

testWorkArea ::
    (Name -> IO b) ->
    Sugar.WorkArea
        (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName)
        InternalName Identity Proxy
        (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName) Proxy) ->
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

workAreaGlobals :: IO ()
workAreaGlobals =
    Sugar.WorkArea
    { Sugar._waPanes =
        -- 2 defs sharing the same tag with different Vars/UUIDs,
        -- should collide with ordinary suffixes
        [ Stub.def "def1" "def" trivialBinder & Stub.pane
        , Stub.def "def2" "def" trivialBinder & Stub.pane
        ]
    , Sugar._waGlobals = Sugar.Globals (pure []) (pure []) (pure [])
    , Sugar._waOpenPane = const Proxy
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
    Annotated (Sugar.Payload (Sugar.Annotation v InternalName) Proxy) #
    Sugar.Assignment
        (Sugar.Annotation (Sugar.EvaluationScopes InternalName Identity) InternalName)
        InternalName Identity Proxy
trivialBinder =
    Sugar.Hole mempty mempty & Sugar.LeafHole & Sugar.BodyLeaf & Sugar.BinderTerm
    & Sugar.Binder Proxy & Sugar.AssignPlain Proxy
    & Sugar.BodyPlain
    & Ann (Const Stub.payload)

anonGlobals :: IO ()
anonGlobals =
    Sugar.WorkArea
    { Sugar._waPanes =
        -- 2 defs sharing the same tag with different Vars/UUIDs,
        -- should collide with ordinary suffixes
        [ Stub.def "def1" anonTag trivialBinder & Stub.pane
        , Stub.def "def2" anonTag trivialBinder & Stub.pane
        ]
    , Sugar._waGlobals = Sugar.Globals (pure []) (pure []) (pure [])
    , Sugar._waOpenPane = const Proxy
    } & testWorkArea (\x -> length (show x) `seq` pure ())
