module Pawl.Codec.CounterSubjectSpec where

import qualified Pawl.Codec.CounterSubject as CounterSubject
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterSubject as CounterSubject

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterSubject" $ do
  Spec.it s "ByEffect (Doubling Season)" $
    Common.assertCodec
      s
      CounterSubject.codec
      CounterSubject.ByEffect
      " {\"type\":\"ByEffect\"} "
  Spec.it s "ByPlayer (Vorinclex, Monstrous Raider)" $
    Common.assertCodec
      s
      CounterSubject.codec
      (CounterSubject.ByPlayer ControllerRelation.Opponents)
      " {\"type\":\"ByPlayer\",\"value\":{\"type\":\"Opponents\"}} "
  Spec.it s "ByAnything (Hardened Scales)" $
    Common.assertCodec
      s
      CounterSubject.codec
      CounterSubject.ByAnything
      " {\"type\":\"ByAnything\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CounterSubject.codec
