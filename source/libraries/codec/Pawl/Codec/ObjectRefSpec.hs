{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ObjectRefSpec where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ObjectRef" $ do
  Spec.it s "InSlot" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      """ "target" """
  Spec.it s "EachMatching" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))
      """ {"type":"HasCardType","value":{"type":"Creature"}} """
  Spec.it s "EachPlayer" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      ObjectRef.EachPlayer
      """ ["EachPlayer"] """
  -- Guards against a decoder that read every payload as one arm regardless of
  -- its JSON type.
  Spec.it s "the three arms are told apart by JSON type, not by a tag" $
    Spec.assertEqWith
      s
      "a slot, a swept set and the player sweep all encode differently"
      ( length
          ( List.nub
              [ ObjectRef.toJson (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
                ObjectRef.toJson (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)),
                ObjectRef.toJson ObjectRef.EachPlayer
              ]
          )
      )
      3
  -- An array the decoder does not know is an error rather than a silent slot.
  Spec.it s "an unknown array word is rejected" $
    Spec.assertBool
      s
      (either (const True) (const False) (Common.parse (Text.pack "[\"EachOpponent\"]") >>= ObjectRef.fromJson))
      "the decoder refuses a word it does not define"
