module Pawl.Codec.EntryRewriteSpec where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect.Type
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRewrite" $ do
  -- CR 707.5: Clone becomes a copy as it enters, excepting nothing -- so the
  -- exception list is omitted and only the eligible filter is written.
  Spec.it s "AsCopy (Clone)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.AsCopy (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) []))
      " {\"type\":\"AsCopy\",\"value\":{\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- CR 707.9b / 707.9d: Quicksilver Gargantuan's "except it's 7/7".
  Spec.it s "AsCopy with an exception (Quicksilver Gargantuan)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.AsCopy (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) [CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 7 7)]))
      " {\"type\":\"AsCopy\",\"value\":{\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"exceptions\":[{\"type\":\"SetPowerToughness\",\"value\":{\"power\":7,\"toughness\":7}}]}} "
  -- The narrowed set the widening exists for (#1512): Copy Enchantment's "any
  -- enchantment", where Clone's is "any creature".
  Spec.it s "AsCopy with a narrower eligible set (Copy Enchantment)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.AsCopy (AsCopy.MkAsCopy (Filter.HasCardType CardType.Enchantment) []))
      " {\"type\":\"AsCopy\",\"value\":{\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Enchantment\"}}}} "
  -- CR 208.2b: two P/T-and-keyword choices, enough to show the keyword union
  -- isn't lost on the wire.
  Spec.it s "ChoiceOf (Primal Plasma)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      ( EntryRewrite.ChoiceOf
          [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
            EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
          ]
      )
      " {\"type\":\"ChoiceOf\",\"value\":[{\"power\":3,\"toughness\":3},{\"power\":1,\"toughness\":6,\"keywords\":[{\"type\":\"Defender\"}]}]} "
  -- CR 614.1c / 105.1: an as-enters colour choice, payload-free because the
  -- five colours are always the whole offer.
  Spec.it s "ChooseColor (Painter's Servant)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.ChooseColor
      " {\"type\":\"ChooseColor\"} "
  Spec.it s "ChooseBasicLandType (Convincing Mirage)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.ChooseBasicLandType
      " {\"type\":\"ChooseBasicLandType\"} "
  -- CR 614.1c / 102.1: an as-enters player choice, payload-free because every
  -- player in the game is the offer and no card narrows it.
  Spec.it s "ChoosePlayer (Stuffy Doll)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.ChoosePlayer
      " {\"type\":\"ChoosePlayer\"} "
  -- CR 614.1c / 201.4a: an as-enters name choice, carrying the restriction on
  -- which cards' names may be chosen -- Null Chamber's "other than a basic land
  -- card name", which is a supertype and a card type together.
  Spec.it s "ChooseCardNames (Null Chamber)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.ChooseCardNames (Filter.Not (Filter.And [Filter.HasSupertype Supertype.Basic, Filter.HasCardType CardType.Land])))
      " {\"type\":\"ChooseCardNames\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasSupertype\",\"value\":{\"type\":\"Basic\"}},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}]}}} "
  -- CR 614.1c / 306.5b: a planeswalker's intrinsic entry-with-counters rewrite.
  Spec.it s "WithCounters (planeswalker loyalty)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Loyalty 3))
      " {\"type\":\"WithCounters\",\"value\":{\"kind\":{\"type\":\"Loyalty\"},\"amount\":3}} "
  -- CR 702.136a: riot's rewrite, payload-free because rule 702.136a fixes both
  -- halves. Minted from a keyword rather than written by a card, and round-tripped
  -- anyway, because every arm of this type is.
  Spec.it s "Riot (Zhur-Taa Goblin)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.Riot
      " {\"type\":\"Riot\"} "
  -- CR 702.98a: unleash's rewrite, riot's without the declining half, and
  -- payload-free for the same reason. Encoded distinctly from Riot, since a
  -- transcript of one must not decode as the other.
  Spec.it s "Unleash (Gore-House Chainwalker)" $ do
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.Unleash
      " {\"type\":\"Unleash\"} "
    Spec.assertBool s (Codec.encode (EntryRewrite.codec (Effect.codec Card.codec)) EntryRewrite.Unleash /= Codec.encode (EntryRewrite.codec (Effect.codec Card.codec)) EntryRewrite.Riot) "unleash and riot encode differently"
  -- CR 702.54a: bloodthirst's rewrite, which DOES carry its N -- the printed
  -- number varies by card, where rule 702.136a fixes riot's. Encoded distinctly
  -- from WithCounters, whose payload names a counter kind rule 702.54a fixes.
  Spec.it s "Bloodthirst (Bloodrage Vampire)" $ do
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.Bloodthirst 1)
      " {\"type\":\"Bloodthirst\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode (EntryRewrite.codec (Effect.codec Card.codec)) (EntryRewrite.Bloodthirst 1) /= Codec.encode (EntryRewrite.codec (Effect.codec Card.codec)) (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne 1)))
      "bloodthirst 1 is not an unconditional +1/+1 counter"
  -- CR 614.1d: the tap-state rewrite a permanent's own text prints, payload-free
  -- because rule 614.1d and CR 110.5b fix both halves.
  Spec.it s "Tapped (Zof Bloodbog)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.Tapped
      " {\"type\":\"Tapped\"} "
  -- CR 712.13a via CR 702.145b: the enters-transformed rewrite, payload-free
  -- because the rule fixes every half -- which face comes from the card's layout,
  -- and the condition from the game's designation.
  Spec.it s "EntersTransformed (Infestation Expert)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.EntersTransformed
      " {\"type\":\"EntersTransformed\"} "
  -- CR 614.1c: the same tap-state rewrite with a price on avoiding it, and the
  -- amount is card text rather than a rule's, so unlike Tapped above it carries a
  -- payload.
  Spec.it s "PayLifeOrTapped (Razorgrass Field)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.PayLifeOrTapped 3)
      " {\"type\":\"PayLifeOrTapped\",\"value\":3} "
  -- CR 614.1c: the same tap-state rewrite again, avoided by revealing a card
  -- instead of by paying life. The payload is which card in the hand qualifies,
  -- which is card text as PayLifeOrTapped's amount is.
  Spec.it s "RevealOrTapped (Rustic Clachan)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.RevealOrTapped (Filter.HasSubtype Subtype.Kithkin))
      " {\"type\":\"RevealOrTapped\",\"value\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Kithkin\"}}} "
  -- CR 616.1b: a control rewrite, payload-free because CR 109.5 derives the
  -- player.
  Spec.it s "UnderSourceControl (Gather Specimens)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      EntryRewrite.UnderSourceControl
      " {\"type\":\"UnderSourceControl\"} "
  -- CR 614.1c / 701.21a: an as-enters sacrifice of any number, carrying both the
  -- criterion the chosen permanents must match -- Shimatsu the Bloodcloaked's is
  -- "any number of permanents", the empty conjunction -- and the counter kind the
  -- count buys.
  Spec.it s "SacrificeAnyNumber (Shimatsu the Bloodcloaked)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber (Filter.And []) (Just CounterKind.PlusOnePlusOne)))
      " {\"type\":\"SacrificeAnyNumber\",\"value\":{\"filter\":{\"type\":\"And\",\"value\":[]},\"kind\":{\"type\":\"PlusOnePlusOne\"}}} "
  -- The same rewrite buying no counters: Wood Elemental's count is read back by
  -- a characteristic-defining ability instead (CR 208.2a), so the second element
  -- is null. Its criterion is the narrowing one -- an untapped Forest (CR 110.5).
  Spec.it s "SacrificeAnyNumber with no counters (Wood Elemental)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber (Filter.And [Filter.HasSubtype Subtype.Forest, Filter.Not Filter.IsTapped]) Nothing))
      " {\"type\":\"SacrificeAnyNumber\",\"value\":{\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Forest\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsTapped\"}}]},\"kind\":null}} "
  -- CR 614.1c: the rewrite that runs an effect -- Monstrous War-Leech's "mill
  -- four cards". Its "if it was kicked" is NOT here: that clause rides
  -- Pawl.Types.PrintedReplacement one level up (CR 604.2).
  Spec.it s "RunEffects (Monstrous War-Leech)" $
    Common.assertCodec
      s
      (EntryRewrite.codec (Effect.codec Card.codec))
      (EntryRewrite.RunEffects (Seq.fromList [Effect.Type.Mill (Mill.MkMill (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 4) Nothing)]))
      " {\"type\":\"RunEffects\",\"value\":[{\"type\":\"Mill\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":4}}}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (EntryRewrite.codec (Effect.codec Card.codec))
