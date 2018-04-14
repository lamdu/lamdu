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
import qualified Lamdu.Name as Name
import           Lamdu.Sugar.Names.Add (InternalName(..), addToWorkArea)
import           Lamdu.Sugar.Names.CPS (liftCPS)
import qualified Lamdu.Sugar.Names.Walk as Walk
import qualified Lamdu.Sugar.Types as Sugar
import           Test.Lamdu.Instances ()
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertString)

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
    addToWorkArea getNameProp inputWorkArea
    & runIdentity
    & getNames
    & traverse_ verifyName
    & testCase "disambiguation"
    where
        verifyName name =
            case Name.visible name of
            (_, Name.NoCollision) -> pure ()
            (Name.TagText text _, _) ->
                assertString ("Unexpected collision for name " ++ show text)

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

inputWorkArea :: Sugar.WorkArea InternalName Identity Unit ()
inputWorkArea =
    Sugar.WorkArea
    { Sugar._waRepl =
        Sugar.Repl
        { Sugar._replExpr = lamExpr
        , Sugar._replResult = CurAndPrev Nothing Nothing
        }
    , Sugar._waPanes =
        [ Sugar.Pane
            { Sugar._paneDefinition =
                Sugar.Definition
                { Sugar._drName =
                    Sugar.Tag
                    { Sugar._tagSelection = tagSelection
                    , Sugar._tagInfo =
                        Sugar.TagInfo
                        { Sugar._tagName =
                            InternalName
                            { _inContext = Just "def"
                            , _inTag = "def"
                            }
                        , Sugar._tagInstance = "dummy"
                        , Sugar._tagVal = "def"
                        }
                    }
                , Sugar._drDefI = "def"
                , Sugar._drDefinitionState = Property Sugar.LiveDefinition (const (Const ())) & Identity
                , Sugar._drEntityId = "dummy"
                , Sugar._drBody =
                    Sugar.DefinitionBodyExpression Sugar.DefinitionExpression
                    { Sugar._deType =
                        Sugar.Scheme
                        { Sugar._schemeForAll = mempty
                        , Sugar._schemeConstraints = mempty
                        , Sugar._schemeType = lamType
                        }
                    , Sugar._dePresentationMode = Nothing
                    , Sugar._deContent = binder "paneVar"
                    }
                }
            , Sugar._paneClose = Const ()
            , Sugar._paneMoveDown = Nothing
            , Sugar._paneMoveUp = Nothing
            }
        ]
    }

lamExpr :: Sugar.Expression InternalName Identity Unit ()
lamExpr =
    Sugar.Expression
    { Sugar._rBody =
        Sugar.BodyLam Sugar.Lambda
        { Sugar._lamMode = Sugar.NormalBinder
        , Sugar._lamBinder = binder "lamVar"
        }
    , Sugar._rPayload = mkPayload lamType
    }

binder :: UUID -> Sugar.Binder InternalName Identity Unit (Sugar.Expression InternalName Identity Unit ())
binder var =
    Sugar.Binder
    { Sugar._bChosenScopeProp = Property Nothing (const (Const ())) & Identity
    , Sugar._bLamId = Just "dummy"
    , Sugar._bBodyScopes = CurAndPrev mempty mempty & Sugar.BinderBodyScope
    , Sugar._bActions =
        Sugar.BinderActions
        { Sugar._baAddFirstParam = Sugar.PrependParam tagSelection
        , Sugar._baMNodeActions = Just nodeActions
        }
    , Sugar._bParams =
        Sugar.Params
        [ Sugar.FuncParam
            { Sugar._fpAnnotation =
                Sugar.Annotation
                { Sugar._aInferredType = numType
                , Sugar._aMEvaluationResult = CurAndPrev Nothing Nothing
                }
            , Sugar._fpInfo =
                Sugar.ParamInfo
                { Sugar._piTag =
                    Sugar.Tag
                    { Sugar._tagSelection = tagSelection
                    , Sugar._tagInfo =
                        Sugar.TagInfo
                        { Sugar._tagName = numName var
                        , Sugar._tagInstance = "dummy"
                        , Sugar._tagVal = "num"
                        }
                    }
                , Sugar._piActions =
                    Sugar.FuncParamActions
                    { Sugar._fpAddNext = Sugar.AddNext tagSelection
                    , Sugar._fpDelete = Const ()
                    , Sugar._fpMOrderBefore = Nothing
                    , Sugar._fpMOrderAfter = Nothing
                    }
                }
            }
        ]
    , Sugar._bBody =
        Sugar.BinderBody
        { Sugar._bbAddOuterLet = Const ()
        , Sugar._bbContent = Sugar.BinderExpr leafExpr
        }
    }

lamType :: Sugar.Type InternalName
lamType =
    Sugar.Type
    { Sugar._tPayload = "dummy"
    , Sugar._tBody = Sugar.TFun numType numType
    }

leafExpr :: Sugar.Expression InternalName Identity Unit ()
leafExpr =
    Sugar.Expression
    { Sugar._rBody = Sugar.BodyPlaceHolder
    , Sugar._rPayload = mkPayload numType
    }

numType :: Sugar.Type InternalName
numType =
    Sugar.Type
    { Sugar._tPayload = "dummy"
    , Sugar._tBody = Sugar.TInst (Sugar.TId (numName "numTid") "num") mempty
    }

mkPayload :: Sugar.Type name -> Sugar.Payload name Identity Unit ()
mkPayload typ =
    Sugar.Payload
    { Sugar._plAnnotation =
        Sugar.Annotation
        { Sugar._aInferredType = typ
        , Sugar._aMEvaluationResult = CurAndPrev Nothing Nothing
        }
    , Sugar._plEntityId = "dummy"
    , Sugar._plActions = nodeActions
    , Sugar._plData = ()
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

numName :: UUID -> InternalName
numName ctx =
    InternalName
    { _inContext = Just ctx
    , _inTag = "num"
    }

tagSelection :: Sugar.TagSelection name Identity Unit ()
tagSelection =
    Sugar.TagSelection
    { Sugar._tsOptions = Identity []
    , Sugar._tsNewTag = const (Const ())
    , Sugar._tsAnon = Nothing
    }
