{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ObjectRefSpec where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ObjectRef" $ do
  Spec.it s "InSlot" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"InSlot","value":"target"} """
  Spec.it s "EachMatching" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))
      """ {"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- The one arm with two payloads, so it keeps an array -- inside the tag's
  -- value now, rather than being the array that carried the tag.
  Spec.it s "EachCardInGraveyard" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.EachCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature))
      """ {"type":"EachCardInGraveyard","value":[{"type":"EachPlayer"},{"type":"HasCardType","value":{"type":"Creature"}}]} """
  Spec.it s "EachPlayer" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      ObjectRef.EachPlayer
      """ {"type":"EachPlayer"} """
  Spec.it s "TopOfLibrary" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.TopOfLibrary (PlayerRef.Relative PlayerRelation.You))
      """ {"type":"TopOfLibrary","value":{"type":"Relative","value":{"type":"You"}}} """
  -- Guards against a decoder that read every payload as one arm. The arms are
  -- now all objects, so only the tag separates them, and a duplicated tag would
  -- collapse two of these.
  Spec.it s "the five arms carry five distinct tags" $
    Spec.assertEqWith
      s
      "a slot, a battlefield sweep, a graveyard sweep, the player sweep and a library's top card all encode differently"
      ( length
          ( List.nub
              [ ObjectRef.toJson (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
                ObjectRef.toJson (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)),
                ObjectRef.toJson (ObjectRef.EachCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature)),
                ObjectRef.toJson ObjectRef.EachPlayer,
                ObjectRef.toJson (ObjectRef.TopOfLibrary (PlayerRef.Relative PlayerRelation.You))
              ]
          )
      )
      5
  -- A tag the decoder does not know is an error rather than a silent slot.
  Spec.it s "an unknown tag is rejected" $
    Spec.assertBool
      s
      (either (const True) (const False) (Common.parse (Text.pack """ {"type":"EachOpponent"} """) >>= ObjectRef.fromJson))
      "the decoder refuses a tag it does not define"
  -- A bare string was the slot arm's whole spelling before #1304. It is not a
  -- ref at all now, which is what stops a card file written against the old
  -- shape from decoding into something plausible.
  Spec.it s "a bare slot name is rejected" $
    Spec.assertBool
      s
      (either (const True) (const False) (Common.parse (Text.pack """ "target" """) >>= ObjectRef.fromJson))
      "the decoder refuses an untagged slot name"
