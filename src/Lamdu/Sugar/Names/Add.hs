{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, FlexibleInstances #-}

module Lamdu.Sugar.Names.Add
    ( addToWorkArea
    , -- re-export for tests
      addToWorkAreaTest
    , InternalName(..), inTag, inContext, runPasses
    ) where

import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Names.Add.Pass0LoadNames (Pass0LoadNames, runPass0LoadNames, P0Env(..))
import           Lamdu.Sugar.Names.Add.Pass1PropagateUp (Pass1PropagateUp, runPass1PropagateUp)
import           Lamdu.Sugar.Names.Add.Pass2MakeNames (Pass2MakeNames, runPass2MakeNamesInitial)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

runPasses ::
    ( HasCallStack
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Functor i
    ) =>
    env ->
    (T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)) ->
    (a -> Pass0LoadNames i b) ->
    (b -> Pass1PropagateUp i o c) ->
    (c -> Pass2MakeNames i o d) ->
    a -> i d
runPasses env getName f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0LoadNames (P0Env getName) . f0
        pass1 = runPass1PropagateUp . f1
        pass2 (x, p1out) = f2 x & runPass2MakeNamesInitial env p1out

addToWorkArea ::
    ( HasCallStack
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Monad i
    ) =>
    env ->
    (T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)) ->
    WorkArea (Annotation (EvaluationScopes InternalName i) InternalName) InternalName i o
        (Annotation (EvaluationScopes InternalName i) InternalName, a, ConvertPayload m) ->
    i (WorkArea (Annotation (EvaluationScopes Name i) Name) Name i o
        (Annotation (EvaluationScopes Name i) Name, a, ConvertPayload m))
addToWorkArea env getName =
    runPasses env getName f f f
    where
        f = Walk.toWorkArea

-- TODO: Switch to regular type in tests
addToWorkAreaTest ::
    ( HasCallStack
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Monad i
    ) =>
    env ->
    (T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)) ->
    WorkArea (Annotation (EvaluationScopes InternalName i) InternalName) InternalName i o
        (Payload (Annotation (EvaluationScopes InternalName i) InternalName) o) ->
    i (WorkArea (Annotation (EvaluationScopes Name i) Name) Name i o
        (Payload (Annotation (EvaluationScopes Name i) Name) o))
addToWorkAreaTest env getName =
    runPasses env getName f f f
    where
        f = Walk.toWorkAreaTest
