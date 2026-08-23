module Pawl.Codec.PreventionRiderSpec where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.PreventionRider as PreventionRider
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PreventionRider" $ do
  -- CR 615.5: the rider's program plus the environment it must resolve in. The
  -- snapshotted target map is what makes "that creature" nameable turns later,
  -- so it is on the wire rather than re-derived -- CR 400.7 has replaced the
  -- installing spell by then.
  Spec.it s "a rider, its snapshotted targets, and who performs it" $
    Common.assertCodec
      s
      PreventionRider.codec
      PreventionRider.MkPreventionRider
        { PreventionRider.effects = Seq.fromList [Effect.Proliferate],
          PreventionRider.targets =
            Map.singleton
              (SlotName.MkSlotName (Text.pack "target"))
              (Set.singleton (Recipient.ToCreature (ObjectId.MkObjectId 4))),
          PreventionRider.controller = PlayerId.MkPlayerId 1,
          PreventionRider.source = ObjectId.MkObjectId 9
        }
      " {\"effects\":[{\"type\":\"Proliferate\"}],\"targets\":{\"target\":[{\"type\":\"ToCreature\",\"value\":4}]},\"controller\":1,\"source\":9} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PreventionRider.codec
