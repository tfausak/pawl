{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ObjectRefSpec where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
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
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"InSlot","value":"target"} """
  Spec.it s "EachMatching" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))
      """ {"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- A two-payload arm, so it keeps an array -- inside the tag's value now,
  -- rather than being the array that carried the tag. Common.tuple writes the
  -- same two elements the hand-written pair did.
  Spec.it s "EachCardInGraveyard" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature))
      """ {"type":"EachCardInGraveyard","value":[{"type":"EachPlayer"},{"type":"HasCardType","value":{"type":"Creature"}}]} """
  -- Common.tuple rejects any other length, which is what makes the schema's
  -- prefixItems/minItems/maxItems a claim rather than a description. The TOO
  -- LONG case is the discriminating one: a decoder that read the first two
  -- elements and ignored the rest would still reject the short payload, so
  -- testing only that proves nothing about the arm's use of Common.tuple.
  Spec.it s "EachCardInGraveyard rejects a too-short payload" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"EachCardInGraveyard","value":[{"type":"EachPlayer"}]} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  Spec.it s "EachCardInGraveyard rejects a too-long payload" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"EachCardInGraveyard","value":[{"type":"EachPlayer"},{"type":"HasCardType","value":{"type":"Creature"}},{"type":"EachPlayer"}]} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  Spec.it s "EachPlayer" $
    Common.assertCodec
      s
      ObjectRef.codec
      ObjectRef.EachPlayer
      """ {"type":"EachPlayer"} """
  -- The other two-payload arm: whose library, and how deep. A depth ABOVE ONE,
  -- since a 1 is what a decoder that dropped the field would answer anyway.
  Spec.it s "TopOfLibrary" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.TopOfLibrary (PlayerRef.Relative PlayerRelation.You) 3)
      """ {"type":"TopOfLibrary","value":[{"type":"Relative","value":{"type":"You"}},3]} """
  Spec.it s "TopOfLibrary rejects a bare player reference with no depth" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"TopOfLibrary","value":{"type":"Relative","value":{"type":"You"}}} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- CR 401.2 counts cards, so a negative depth is not a number of them.
  Spec.it s "TopOfLibrary rejects a negative depth" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"TopOfLibrary","value":[{"type":"Relative","value":{"type":"You"}},-1]} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- The graveyard's OTHER arm: one card the resolving controller chooses rather
  -- than the whole matching set. Its payload is EachCardInGraveyard's exactly,
  -- so only the tag tells them apart -- which is what the distinctness case
  -- below is for.
  Spec.it s "ChosenCardInGraveyard" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard PlayerScope.You (Filter.HasCardType CardType.Creature))
      """ {"type":"ChosenCardInGraveyard","value":[{"type":"You"},{"type":"HasCardType","value":{"type":"Creature"}}]} """
  Spec.it s "ChosenCardInGraveyard rejects a bare filter with no scope" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"ChosenCardInGraveyard","value":{"type":"HasCardType","value":{"type":"Creature"}}} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- Guards against a decoder that read every payload as one arm. The arms are
  -- all objects, so only the tag separates them, and a duplicated tag would
  -- collapse two of these. The two graveyard arms are the pair it really
  -- guards: they carry the SAME payload, so a copied tag would quietly turn one
  -- card's chosen return into a mass one.
  Spec.it s "the six arms carry six distinct tags" $
    Spec.assertEqWith
      s
      "a slot, a battlefield sweep, a graveyard sweep, the player sweep, a library's top card and a chosen graveyard card all encode differently"
      ( length
          ( List.nub
              [ Codec.encode ObjectRef.codec (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
                Codec.encode ObjectRef.codec (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)),
                Codec.encode ObjectRef.codec (ObjectRef.EachCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature)),
                Codec.encode ObjectRef.codec ObjectRef.EachPlayer,
                Codec.encode ObjectRef.codec (ObjectRef.TopOfLibrary (PlayerRef.Relative PlayerRelation.You) 3),
                Codec.encode ObjectRef.codec (ObjectRef.ChosenCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature))
              ]
          )
      )
      6
  -- A tag the decoder does not know is an error rather than a silent slot.
  Spec.it s "an unknown tag is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"EachOpponent"} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- A bare string was the slot arm's whole spelling before #1304. It is not a
  -- ref at all now, which is what stops a card file written against the old
  -- shape from decoding into something plausible.
  Spec.it s "a bare slot name is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ "target" """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s ObjectRef.codec
