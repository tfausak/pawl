module Pawl.Codec.InherentTriggerSourceSpec where

import qualified Pawl.Codec.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Codec.TriggeredAbilitySourceSpec as TriggeredAbilitySourceSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.InherentTriggerSource" $ do
  -- CR 725.2 / CR 702.179d: no source object, so the first key is the
  -- controller baked in at trigger time. The ability is the one
  -- Pawl.Codec.TriggeredAbilitySourceSpec writes under `source`, so the two
  -- cases differ in exactly the key that separates the records.
  Spec.it s "a sourceless triggered ability and the player controlling it" $
    Common.assertCodec
      s
      InherentTriggerSource.codec
      InherentTriggerSource.MkInherentTriggerSource
        { InherentTriggerSource.controller = PlayerId.MkPlayerId 1,
          InherentTriggerSource.ability = TriggeredAbilitySourceSpec.ability
        }
      " {\"controller\":1,\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s InherentTriggerSource.codec
