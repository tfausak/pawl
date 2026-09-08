module Pawl.Codec.PendingDamageEffectSpec where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.PendingDamageEffect as PendingDamageEffect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PendingDamageEffect as PendingDamageEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PendingDamageEffect" $ do
  -- CR 614.1a: the program, plus the environment snapshotted when the rewrite
  -- applied. The slot map is what makes "that creature" nameable at the drain,
  -- so it is on the wire beside the effects.
  Spec.it s "a queued damage-replacement effect and the environment it runs in" $
    Common.assertCodec
      s
      PendingDamageEffect.codec
      PendingDamageEffect.MkPendingDamageEffect
        { PendingDamageEffect.effects = Seq.fromList [Effect.Proliferate],
          PendingDamageEffect.targets = Map.singleton (SlotName.MkSlotName (Text.pack "creature")) (Set.singleton (Recipient.ToObject (ObjectId.MkObjectId 5))),
          PendingDamageEffect.controller = PlayerId.MkPlayerId 1,
          PendingDamageEffect.source = ObjectId.MkObjectId 3
        }
      " {\"effects\":[{\"type\":\"Proliferate\"}],\"targets\":{\"creature\":[{\"type\":\"ToObject\",\"value\":5}]},\"controller\":1,\"source\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PendingDamageEffect.codec
