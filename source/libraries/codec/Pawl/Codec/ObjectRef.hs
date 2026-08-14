module Pawl.Codec.ObjectRef where

import qualified Pawl.Codec.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Codec.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TopOfLibrary as TopOfLibrary
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
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
-- 'EachCardInGraveyard', 'TopOfLibrary' and 'ChosenCardInGraveyard' each carry a
-- payload record of their own (#1464), so no arm here writes a positional array.
codec :: Codec.Codec ObjectRef.ObjectRef
codec =
  Arm.tagged
    [ Arm.payload "InSlot" SlotName.codec ObjectRef.InSlot (\x -> case x of ObjectRef.InSlot y -> Just y; _ -> Nothing),
      Arm.payload "EachMatching" filterCodec ObjectRef.EachMatching (\x -> case x of ObjectRef.EachMatching y -> Just y; _ -> Nothing),
      Arm.payload "EachCardInGraveyard" EachCardInGraveyard.codec ObjectRef.EachCardInGraveyard (\x -> case x of ObjectRef.EachCardInGraveyard y -> Just y; _ -> Nothing),
      Arm.nullary "EachCardInYourHand" ObjectRef.EachCardInYourHand,
      Arm.nullary "EachPlayer" ObjectRef.EachPlayer,
      Arm.payload "TopOfLibrary" TopOfLibrary.codec ObjectRef.TopOfLibrary (\x -> case x of ObjectRef.TopOfLibrary y -> Just y; _ -> Nothing),
      Arm.payload "ChosenCardInGraveyard" ChosenCardInGraveyard.codec ObjectRef.ChosenCardInGraveyard (\x -> case x of ObjectRef.ChosenCardInGraveyard y -> Just y; _ -> Nothing)
    ]
  where
    -- Written once so the encoder, the decoder and the schema cannot disagree
    -- about which keyword codec the filter carries.
    filterCodec = Filter.codec Keyword.codec
