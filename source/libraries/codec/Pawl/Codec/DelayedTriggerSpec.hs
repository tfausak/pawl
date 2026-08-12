module Pawl.Codec.DelayedTriggerSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.DelayedTrigger as DelayedTrigger
import qualified Pawl.Codec.FaceSpec as FaceSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TurnWindow as TurnWindow

-- | MkDelayedTrigger at CR 603.7a/603.7b's default (an unrestricted window and
-- no expiry), plus the case each of those two fields takes when the card does
-- restrict it.
entry :: DelayedTrigger.DelayedTrigger
entry =
  DelayedTrigger.MkDelayedTrigger
    { DelayedTrigger.ability = FaceSpec.minimalTriggeredAbility,
      DelayedTrigger.source = ObjectId.MkObjectId 4,
      DelayedTrigger.controller = PlayerId.MkPlayerId 0,
      DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.empty {Binding.amount = Just 9}),
      DelayedTrigger.window = TurnWindow.AnyTurn,
      DelayedTrigger.expiry = Nothing
    }

entryJson :: String
entryJson =
  "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,"
    <> "\"bindings\":{\"token\":{\"amount\":9}},"
    <> "\"window\":{\"type\":\"AnyTurn\"}}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DelayedTrigger" $ do
  Spec.it s "MkDelayedTrigger, CR 603.7a/603.7b's default (armed with no onset gate, no stated duration)" $
    Common.assertJsonCodec s DelayedTrigger.toJson DelayedTrigger.fromJson entry entryJson
  -- CR 603.7b: a stated duration as the game remembers it
  -- (Pawl.Engine.Expiry.arm's output, not the printed Duration).
  Spec.it s "MkDelayedTrigger, a stated expiry (CR 603.7b)" $
    Common.assertJsonCodec
      s
      DelayedTrigger.toJson
      DelayedTrigger.fromJson
      entry {DelayedTrigger.expiry = Just Expiry.AtCleanup}
      ( "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,"
          <> "\"bindings\":{\"token\":{\"amount\":9}},"
          <> "\"window\":{\"type\":\"AnyTurn\"},\"expiry\":{\"type\":\"AtCleanup\"}}"
      )
  -- CR 603.7a: an onset gate. Pawl.Codec.TurnWindowSpec covers the other arms.
  Spec.it s "MkDelayedTrigger, an onset gate (CR 603.7a)" $
    Common.assertJsonCodec
      s
      DelayedTrigger.toJson
      DelayedTrigger.fromJson
      entry {DelayedTrigger.window = TurnWindow.ControllersNextTurn}
      ( "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,"
          <> "\"bindings\":{\"token\":{\"amount\":9}},"
          <> "\"window\":{\"type\":\"ControllersNextTurn\"}}"
      )
  -- 'bindings' at its default and no stated 'expiry'. 'window' stays a required
  -- key regardless: CR 603.7a's "no restriction" is one of TurnWindow's arms,
  -- not the absence of one.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      DelayedTrigger.toJson
      DelayedTrigger.fromJson
      DelayedTrigger.MkDelayedTrigger
        { DelayedTrigger.ability = FaceSpec.minimalTriggeredAbility,
          DelayedTrigger.source = ObjectId.MkObjectId 4,
          DelayedTrigger.controller = PlayerId.MkPlayerId 0,
          DelayedTrigger.bindings = Map.empty,
          DelayedTrigger.window = TurnWindow.AnyTurn,
          DelayedTrigger.expiry = Nothing
        }
      "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,\"window\":{\"type\":\"AnyTurn\"}}"
