module Pawl.Codec.ObjectRef where

import qualified Pawl.Codec.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Codec.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TopOfLibrary as TopOfLibrary
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | Tagged like every other sum. The arms were previously told apart by
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
-- 'EachCardInGraveyard' and 'TopOfLibrary' are the arms with two payloads and
-- 'ChosenCardInGraveyard' the one with three, so each takes a 'Common.tuple' or
-- 'Common.tuple3'. Under the #1305 decision they owe records of
-- their own like every other multi-payload arm; that lands with the
-- payload-records unit.
codec :: Codec.Codec ObjectRef.ObjectRef
codec =
  Arm.tagged
    encode
    [ Arm.payload "InSlot" SlotName.codec ObjectRef.InSlot,
      Arm.payload "EachMatching" filterCodec ObjectRef.EachMatching,
      Arm.payload "EachCardInGraveyard" EachCardInGraveyard.codec ObjectRef.EachCardInGraveyard,
      Arm.nullary "EachPlayer" ObjectRef.EachPlayer,
      Arm.payload "TopOfLibrary" TopOfLibrary.codec ObjectRef.TopOfLibrary,
      Arm.payload "ChosenCardInGraveyard" ChosenCardInGraveyard.codec ObjectRef.ChosenCardInGraveyard
    ]
  where
    -- Written once so the encoder, the decoder and the schema cannot disagree
    -- about which keyword codec the filter carries.
    filterCodec = Filter.codec Keyword.codec
    encode r = case r of
      ObjectRef.InSlot n -> Common.tagged "InSlot" . Just $ Codec.encode SlotName.codec n
      ObjectRef.EachMatching f -> Common.tagged "EachMatching" . Just $ Codec.encode filterCodec f
      ObjectRef.EachCardInGraveyard x -> Common.tagged "EachCardInGraveyard" . Just $ Codec.encode EachCardInGraveyard.codec x
      ObjectRef.EachPlayer -> Common.nullary "EachPlayer"
      ObjectRef.TopOfLibrary x -> Common.tagged "TopOfLibrary" . Just $ Codec.encode TopOfLibrary.codec x
      ObjectRef.ChosenCardInGraveyard x -> Common.tagged "ChosenCardInGraveyard" . Just $ Codec.encode ChosenCardInGraveyard.codec x
