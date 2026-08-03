module Pawl.Codec.DelayedTriggerSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.CardSpec as CardSpec
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DelayedTrigger as DelayedTrigger
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TurnWindow as TurnWindow

-- | R7's one case for MkDelayedTrigger's single constructor, at CR 603.7a/603.7b's
-- default (an unrestricted window and no expiry), plus the case each of those two
-- fields takes when the card does restrict it. 'bindings' carries a minimal
-- Binding rather than one with a `copy` snapshot (Pawl.Codec.BindingSpec is
-- already total over every Binding field on its own); 'ability' reuses
-- 'CardSpec.minimalTriggeredAbility' rather than building a second one by hand.
entry :: DelayedTrigger.DelayedTrigger
entry =
  DelayedTrigger.MkDelayedTrigger
    { DelayedTrigger.ability = CardSpec.minimalTriggeredAbility,
      DelayedTrigger.source = ObjectId.MkObjectId 4,
      DelayedTrigger.controller = PlayerId.MkPlayerId 0,
      DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.empty {Binding.amount = Just 9}),
      DelayedTrigger.window = TurnWindow.AnyTurn,
      DelayedTrigger.expiry = Nothing
    }

entryJson :: String
entryJson =
  "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,"
    <> "\"bindings\":[{\"slot\":\"token\",\"binding\":{\"target\":null,\"amount\":9,\"modes\":null,\"copy\":null}}],"
    <> "\"window\":{\"type\":\"AnyTurn\"},\"expiry\":null}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DelayedTrigger" $ do
  Spec.it s "MkDelayedTrigger, CR 603.7a/603.7b's default (armed with no onset gate, no stated duration)" $
    Common.assertJsonCodec s DelayedTrigger.toJson DelayedTrigger.fromJson entry entryJson
  -- CR 603.7b: a stated duration, Full Throttle's "this turn" as the game
  -- remembers it (Pawl.Engine.Expiry.arm's output, not the printed Duration).
  Spec.it s "MkDelayedTrigger, a stated expiry (CR 603.7b)" $
    Common.assertJsonCodec
      s
      DelayedTrigger.toJson
      DelayedTrigger.fromJson
      entry {DelayedTrigger.expiry = Just Expiry.AtCleanup}
      ( "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,"
          <> "\"bindings\":[{\"slot\":\"token\",\"binding\":{\"target\":null,\"amount\":9,\"modes\":null,\"copy\":null}}],"
          <> "\"window\":{\"type\":\"AnyTurn\"},\"expiry\":{\"type\":\"AtCleanup\"}}"
      )
  -- CR 603.7a: an onset gate, at the arm Meandering Towershell's entry is stored
  -- with before its turn arrives. Pawl.Codec.TurnWindowSpec is total over the
  -- other two arms.
  Spec.it s "MkDelayedTrigger, an onset gate (CR 603.7a)" $
    Common.assertJsonCodec
      s
      DelayedTrigger.toJson
      DelayedTrigger.fromJson
      entry {DelayedTrigger.window = TurnWindow.ControllersNextTurn}
      ( "{\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0,"
          <> "\"bindings\":[{\"slot\":\"token\",\"binding\":{\"target\":null,\"amount\":9,\"modes\":null,\"copy\":null}}],"
          <> "\"window\":{\"type\":\"ControllersNextTurn\"},\"expiry\":null}"
      )
