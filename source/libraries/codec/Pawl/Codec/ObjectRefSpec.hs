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
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ObjectRef" $ do
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"InSlot\",\"value\":\"target\"} "
  Spec.it s "EachMatching" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))
      " {\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- A two-payload arm, so its value is a payload record keyed by the field
  -- names (#1464) rather than the positional array it once was.
  Spec.it s "EachCardInGraveyard" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard (GraveyardScope.Scoped PlayerScope.EachPlayer) (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"EachCardInGraveyard\",\"value\":{\"graveyards\":{\"type\":\"Scoped\",\"value\":{\"type\":\"EachPlayer\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- The record codec rejects an ARRAY of any length, which is what keeps the
  -- old positional wire format from decoding silently. Both lengths are asserted
  -- rather than one: a decoder that had kept a tuple fallback would reject the
  -- short payload on arity alone, so the too-long case is what discriminates.
  Spec.it s "EachCardInGraveyard rejects a too-short payload" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"EachCardInGraveyard\",\"value\":[{\"type\":\"EachPlayer\"}]} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  Spec.it s "EachCardInGraveyard rejects a too-long payload" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"EachCardInGraveyard\",\"value\":[{\"type\":\"EachPlayer\"},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"EachPlayer\"}]} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- CR 109.2b: the word "spell" switches CR 109.2's battlefield default to the
  -- stack. Swift Silence's "all other spells" is the filter, and "other" is the
  -- `Not IsSource` every "another" in the pool is already written as.
  Spec.it s "EachSpell" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachSpell (Filter.Not Filter.IsSource))
      " {\"type\":\"EachSpell\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}} "
  Spec.it s "EachPlayer" $
    Common.assertCodec
      s
      ObjectRef.codec
      ObjectRef.EachPlayer
      " {\"type\":\"EachPlayer\"} "
  -- Nullary like EachPlayer above, and for CR 102.1 rather than an economy: the
  -- perspective an opponent is relative to is CR 109.5's "you", which the
  -- resolution supplies, so the arm has no player to carry.
  Spec.it s "EachOpponent" $
    Common.assertCodec
      s
      ObjectRef.codec
      ObjectRef.EachOpponent
      " {\"type\":\"EachOpponent\"} "
  -- Nullary like EachPlayer above, and for a rule: CR 614.12a made the choice as
  -- the source entered, so the ref names it rather than restating it.
  Spec.it s "ChosenPlayer" $
    Common.assertCodec
      s
      ObjectRef.codec
      ObjectRef.ChosenPlayer
      " {\"type\":\"ChosenPlayer\"} "
  -- Nullary like EachPlayer above, and for a rule rather than an economy: CR
  -- 400.2 makes a hand hidden, so this arm names only the resolving
  -- controller's own and carries neither a player nor a filter.
  Spec.it s "EachCardInYourHand" $
    Common.assertCodec
      s
      ObjectRef.codec
      ObjectRef.EachCardInYourHand
      " {\"type\":\"EachCardInYourHand\"} "
  -- No player, for a rule: CR 607.2a's set is defined by which object exiled the
  -- card. The bare tag is the whole linked set, which is what three of the four
  -- printings that read one take.
  Spec.it s "EachCardExiledWithSource" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachCardExiledWithSource Nothing)
      " {\"type\":\"EachCardExiledWithSource\"} "
  -- And the narrowed set, Karn Liberated's "non-Aura permanent cards": an absent
  -- key and a stated filter are different refs, so the optional payload has to
  -- survive the trip rather than being dropped to the bare tag.
  Spec.it s "EachCardExiledWithSource narrowed by a filter" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachCardExiledWithSource (Just (Filter.Not (Filter.HasSubtype Subtype.Aura))))
      " {\"type\":\"EachCardExiledWithSource\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Aura\"}}}} "
  -- The other two-payload arm: whose library, and how deep. A depth ABOVE ONE,
  -- since a 1 is what a decoder that dropped the field would answer anyway.
  Spec.it s "TopOfLibrary" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 3)))
      " {\"type\":\"TopOfLibrary\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"count\":{\"type\":\"Literal\",\"value\":3}}} "
  -- Commune with Lava's "the top X cards of your library": the same arm with a
  -- computed depth, which is what a bare number could never say.
  Spec.it s "TopOfLibrary carries the computed depth Commune with Lava needs" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")))))
      " {\"type\":\"TopOfLibrary\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"count\":{\"type\":\"InSlot\",\"value\":\"X\"}}} "
  -- The walk-terminated sibling: the arm above's keys plus a Filter, which is
  -- the whole difference between them on the wire too -- the shared @count@
  -- counting matches here where it counts cards there.
  Spec.it s "TopOfLibraryUntil" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil (PlayerRef.Relative PlayerRelation.You) (Filter.Not (Filter.HasCardType CardType.Land)) (Quantity.Literal 1)))
      " {\"type\":\"TopOfLibraryUntil\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}},\"count\":{\"type\":\"Literal\",\"value\":1}}} "
  Spec.it s "TopOfLibrary rejects a bare player reference with no depth" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"TopOfLibrary\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- The depth was a bare number before it became a Quantity, so a card file
  -- written against that shape fails to decode rather than reading as a literal.
  -- A NEGATIVE depth is no longer a decode failure -- Quantity.Literal takes an
  -- Integer -- and is clamped to zero where it is read instead (CR 107.1b),
  -- which Pawl.Engine.Resolve.objectRefObjects' own note records.
  Spec.it s "TopOfLibrary rejects a bare number as a depth" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"TopOfLibrary\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"count\":3}} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- The graveyard's OTHER arm: a card somebody chooses rather than the whole
  -- matching set. Its filter is EachCardInGraveyard's exactly and its scope is
  -- the PlayerScope that arm used to carry, so the tag, the leading chooser and
  -- the scope's own shape tell them apart -- which is what the distinctness case
  -- below is for; see #1952 for what this arm still cannot say that the sweep
  -- now can.
  Spec.it s "ChosenCardInGraveyard" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController PlayerScope.You (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"ChosenCardInGraveyard\",\"value\":{\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "ChosenCardInGraveyard carries the chooser Exhume needs" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.EachInScope PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"ChosenCardInGraveyard\",\"value\":{\"chooser\":{\"type\":\"EachInScope\"},\"players\":{\"type\":\"EachPlayer\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "ChosenCardInGraveyard carries the slot-named chooser Skullwinder needs" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard (Chooser.BoundInSlot (SlotName.MkSlotName (Text.pack "opponent"))) PlayerScope.EachPlayer (Filter.And [])))
      " {\"type\":\"ChosenCardInGraveyard\",\"value\":{\"chooser\":{\"type\":\"BoundInSlot\",\"value\":\"opponent\"},\"players\":{\"type\":\"EachPlayer\"},\"filter\":{\"type\":\"And\",\"value\":[]}}} "
  Spec.it s "ChosenCardInGraveyard rejects a bare filter with no chooser or scope" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"ChosenCardInGraveyard\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- The chooser is REQUIRED rather than defaulted, so a card written before it
  -- existed is a decode failure rather than a silent controller choice.
  Spec.it s "ChosenCardInGraveyard rejects the two-element payload that preceded the chooser" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"ChosenCardInGraveyard\",\"value\":[{\"type\":\"You\"},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}]} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- Karn Liberated's "+4: target player exiles a card from their hand". One
  -- PlayerRef, since CR 402.3 makes the chooser and the hand's owner the same
  -- player -- and the slot it names is the one Karn targets. Karn states no
  -- characteristic, so its filter is the always-matching one.
  Spec.it s "ChosenCardInHand" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Filter.And [])))
      " {\"type\":\"ChosenCardInHand\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"filter\":{\"type\":\"And\",\"value\":[]}}} "
  -- Elvish Piper's "a creature card from your hand": the same arm with the
  -- Filter stating a characteristic, which is the pair Karn's unfiltered wording
  -- cannot tell apart.
  Spec.it s "ChosenCardInHand carries the filter Elvish Piper needs" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand (PlayerRef.Relative PlayerRelation.You) (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"ChosenCardInHand\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- The filter is REQUIRED rather than defaulted, so a card written before it
  -- existed is a decode failure rather than a silently unnarrowed choice.
  Spec.it s "ChosenCardInHand rejects the bare player reference that preceded the filter" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"ChosenCardInHand\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- Commune with the Gods' "a creature or enchantment card from among them": the
  -- slot names the group an earlier clause bound, where the two arms above name a
  -- zone.
  Spec.it s "ChosenCardFromAmong" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong (SlotName.MkSlotName (Text.pack "revealed")) (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"ChosenCardFromAmong\",\"value\":{\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- The bare slot name an InSlot takes is not a whole payload here: dropping the
  -- filter would make the two arms tell one story on the wire.
  -- The plural sibling: the same two keys, a different tag.
  Spec.it s "EachCardFromAmong" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong (SlotName.MkSlotName (Text.pack "revealed")) (Filter.HasCardType CardType.Land)))
      " {\"type\":\"EachCardFromAmong\",\"value\":{\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  Spec.it s "ChosenCardFromAmong rejects a bare slot name" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"ChosenCardFromAmong\",\"value\":\"revealed\"} ") >>= Codec.decode ObjectRef.codec))
      "a bare slot name is rejected"
  -- Merfolk Spy's "that player reveals a card at random from their hand": the
  -- chosen arm's PlayerRef with no filter beside it (gap #1742), and the slot is the
  -- one the trigger bound to the damaged player.
  Spec.it s "RandomCardInHand" $
    Common.assertCodec
      s
      ObjectRef.codec
      (ObjectRef.RandomCardInHand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))))
      " {\"type\":\"RandomCardInHand\",\"value\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"}} "
  -- Guards against a decoder that read every payload as one arm. The arms are
  -- all objects, so only the tag separates them, and a duplicated tag would
  -- collapse two of these. The two graveyard arms are the pair it really
  -- guards: they carry the SAME payload, so a copied tag would quietly turn one
  -- card's chosen return into a mass one. The two HAND arms are a second such
  -- pair now -- a chosen card and a random one over the same zone -- and a
  -- copied tag there would turn randomness into a player's decision.
  -- The two PLAYER arms are a third such pair: a sweep of every seat and the one
  -- seat the source chose, so a copied tag would spray Stuffy Doll's damage over
  -- the table.
  Spec.it s "every arm carries a distinct tag" $
    Spec.assertEqWith
      s
      "a slot, a battlefield sweep, a graveyard sweep, the hand sweep, the linked exile sweep, the stack sweep, the player sweep, the opponent sweep, the chosen player, a library's top cards, a walk of a library, a chosen graveyard card, a chosen card in hand, a chosen card from among a group, every card from among a group and a random card in hand all encode differently"
      ( length
          ( List.nub
              [ Codec.encode ObjectRef.codec (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
                Codec.encode ObjectRef.codec (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)),
                Codec.encode ObjectRef.codec (ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard (GraveyardScope.Scoped PlayerScope.EachPlayer) (Filter.HasCardType CardType.Creature))),
                Codec.encode ObjectRef.codec ObjectRef.EachCardInYourHand,
                Codec.encode ObjectRef.codec (ObjectRef.EachCardExiledWithSource Nothing),
                Codec.encode ObjectRef.codec (ObjectRef.EachSpell (Filter.Not Filter.IsSource)),
                Codec.encode ObjectRef.codec ObjectRef.EachPlayer,
                Codec.encode ObjectRef.codec ObjectRef.EachOpponent,
                Codec.encode ObjectRef.codec ObjectRef.ChosenPlayer,
                Codec.encode ObjectRef.codec (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 3))),
                Codec.encode ObjectRef.codec (ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil (PlayerRef.Relative PlayerRelation.You) (Filter.Not (Filter.HasCardType CardType.Land)) (Quantity.Literal 1))),
                Codec.encode ObjectRef.codec (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController PlayerScope.EachPlayer (Filter.HasCardType CardType.Creature))),
                Codec.encode ObjectRef.codec (ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand (PlayerRef.Relative PlayerRelation.You) (Filter.HasCardType CardType.Creature))),
                Codec.encode ObjectRef.codec (ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong (SlotName.MkSlotName (Text.pack "revealed")) (Filter.HasCardType CardType.Creature))),
                Codec.encode ObjectRef.codec (ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong (SlotName.MkSlotName (Text.pack "revealed")) (Filter.HasCardType CardType.Land))),
                Codec.encode ObjectRef.codec (ObjectRef.RandomCardInHand (PlayerRef.Relative PlayerRelation.You))
              ]
          )
      )
      16
  -- A tag the decoder does not know is an error rather than a silent slot. The
  -- tag has to be one no arm will ever claim -- @EachOpponent@ stood here until
  -- that became a real arm, and the case then failed rather than going quiet,
  -- which is the direction this wants to fail in.
  Spec.it s "an unknown tag is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"NotAnObjectRef\"} ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  -- A bare string was the slot arm's whole spelling before #1304. It is not a
  -- ref at all now, which is what stops a card file written against the old
  -- shape from decoding into something plausible.
  Spec.it s "a bare slot name is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " \"target\" ") >>= Codec.decode ObjectRef.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s ObjectRef.codec
