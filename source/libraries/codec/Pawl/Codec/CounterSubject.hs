module Pawl.Codec.CounterSubject where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CounterSubject as CounterSubject

codec :: Codec.Codec CounterSubject.CounterSubject
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "ByEffect" CounterSubject.ByEffect,
      Arm.payload "ByPlayer" ControllerRelation.codec CounterSubject.ByPlayer (\x -> case x of CounterSubject.ByPlayer y -> Just y; _ -> Nothing),
      Arm.nullary "ByAnything" CounterSubject.ByAnything
    ]

tagOf :: CounterSubject.CounterSubject -> String
tagOf x = case x of
  CounterSubject.ByEffect {} -> "ByEffect"
  CounterSubject.ByPlayer {} -> "ByPlayer"
  CounterSubject.ByAnything {} -> "ByAnything"
