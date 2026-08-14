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
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary

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
  -- A two-payload arm, so its value is a payload record keyed by the field
  -- names (#1464) rather than the positional array it once was.
  Spec.it s "EachCardInGraveyard" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature)))
      """ {"type":"EachCardInGraveyard","value":{"players":{"type":"EachPlayer"},"filter":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  -- The record codec rejects an ARRAY of any length, which is what keeps the
  -- old positional wire format from decoding silently. Both lengths are asserted
  -- rather than one: a decoder that had kept a tuple fallback would reject the
  -- short payload on arity alone, so the too-long case is what discriminates.
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
  -- Nullary like EachPlayer above, and for a rule rather than an economy: CR
  -- 400.2 makes a hand hidden, so this arm names only the resolving
  -- controller's own and carries neither a player nor a filter.
  Spec.it s "EachCardInYourHand" $
    Common.assertCodec
      s
      ObjectRef.codec
      ObjectRef.EachCardInYourHand
      """ {"type":"EachCardInYourHand"} """
  -- The other two-payload arm: whose library, and how deep. A depth ABOVE ONE,
  -- since a 1 is what a decoder that dropped the field would answer anyway.
  Spec.it s "TopOfLibrary" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) 3))
      """ {"type":"TopOfLibrary","value":{"player":{"type":"Relative","value":{"type":"You"}},"count":3}} """
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
  -- The graveyard's OTHER arm: a card somebody chooses rather than the whole
  -- matching set. Its scope and filter are EachCardInGraveyard's exactly, so
  -- only the tag and the leading chooser tell them apart -- which is what the
  -- distinctness case below is for.
  Spec.it s "ChosenCardInGraveyard" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController PlayerScope.You (Filter.HasCardType CardType.Creature)))
      """ {"type":"ChosenCardInGraveyard","value":{"chooser":{"type":"TheController"},"players":{"type":"You"},"filter":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  Spec.it s "ChosenCardInGraveyard carries the chooser Exhume needs" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.EachInScope PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature)))
      """ {"type":"ChosenCardInGraveyard","value":{"chooser":{"type":"EachInScope"},"players":{"type":"EachPlayer"},"filter":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  Spec.it s "ChosenCardInGraveyard carries the slot-named chooser Skullwinder needs" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard (Chooser.BoundInSlot (SlotName.MkSlotName (Text.pack "opponent"))) PlayerScope.EachPlayer (Filter.And [])))
      """ {"type":"ChosenCardInGraveyard","value":{"chooser":{"type":"BoundInSlot","value":"opponent"},"players":{"type":"EachPlayer"},"filter":{"type":"And","value":[]}}} """
  Spec.it s "ChosenCardInGraveyard rejects a bare filter with no chooser or scope" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"ChosenCardInGraveyard","value":{"type":"HasCardType","value":{"type":"Creature"}}} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- The chooser is REQUIRED rather than defaulted, so a card written before it
  -- existed is a decode failure rather than a silent controller choice.
  Spec.it s "ChosenCardInGraveyard rejects the two-element payload that preceded the chooser" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"ChosenCardInGraveyard","value":[{"type":"You"},{"type":"HasCardType","value":{"type":"Creature"}}]} """) >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- Guards against a decoder that read every payload as one arm. The arms are
  -- all objects, so only the tag separates them, and a duplicated tag would
  -- collapse two of these. The two graveyard arms are the pair it really
  -- guards: they carry the SAME payload, so a copied tag would quietly turn one
  -- card's chosen return into a mass one.
  Spec.it s "the seven arms carry seven distinct tags" $
    Spec.assertEqWith
      s
      "a slot, a battlefield sweep, a graveyard sweep, the hand sweep, the player sweep, a library's top card and a chosen graveyard card all encode differently"
      ( length
          ( List.nub
              [ Codec.encode ObjectRef.codec (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
                Codec.encode ObjectRef.codec (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)),
                Codec.encode ObjectRef.codec (ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature))),
                Codec.encode ObjectRef.codec ObjectRef.EachCardInYourHand,
                Codec.encode ObjectRef.codec ObjectRef.EachPlayer,
                Codec.encode ObjectRef.codec (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) 3)),
                Codec.encode ObjectRef.codec (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature)))
              ]
          )
      )
      7
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
