module Pawl.Codec.ObjectRef where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | Tagged like every other sum. The five arms were previously told apart by
-- JSON TYPE -- a string was a slot, an object the battlefield sweep's Filter,
-- and everything else an array leading with its own word -- which no schema can
-- state as a claim the decoder guarantees (#1304). The word an array led with
-- was already a tag in all but name; it now sits under @type@ where every other
-- sum's does, and the arms carrying no payload or one need no array at all.
--
-- A BUNDLE since 'Pawl.Codec.PlayerRef' became one, which is what 'TopOfLibrary'
-- was waiting on. The wire format is unchanged by that conversion -- the same
-- five tags, emitted identically -- and what it adds is the schema.
--
-- 'EachCardInGraveyard' is the one arm with two payloads, so it takes a
-- 'Common.tuple'. Under the #1305 decision it owes a record of its own like
-- every other multi-payload arm; that lands with the payload-records unit.
codec :: Codec.Codec ObjectRef.ObjectRef
codec =
  Arm.tagged
    encode
    [ Arm.payload "InSlot" SlotName.codec ObjectRef.InSlot,
      Arm.payload "EachMatching" filterCodec ObjectRef.EachMatching,
      Arm.payload "EachCardInGraveyard" (Common.tuple PlayerScope.codec filterCodec) (uncurry ObjectRef.EachCardInGraveyard),
      Arm.nullary "EachPlayer" ObjectRef.EachPlayer,
      Arm.payload "TopOfLibrary" PlayerRef.codec ObjectRef.TopOfLibrary
    ]
  where
    -- Written once so the encoder, the decoder and the schema cannot disagree
    -- about which keyword codec the filter carries.
    filterCodec = Filter.codec Keyword.codec
    encode r = case r of
      ObjectRef.InSlot n -> Common.tagged "InSlot" . Just $ Codec.encode SlotName.codec n
      ObjectRef.EachMatching f -> Common.tagged "EachMatching" . Just $ Codec.encode filterCodec f
      ObjectRef.EachCardInGraveyard s f ->
        Common.tagged "EachCardInGraveyard" . Just . Value.array $
          [Codec.encode PlayerScope.codec s, Codec.encode filterCodec f]
      ObjectRef.EachPlayer -> Common.nullary "EachPlayer"
      ObjectRef.TopOfLibrary p -> Common.tagged "TopOfLibrary" . Just $ Codec.encode PlayerRef.codec p
