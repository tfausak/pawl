module Pawl.Types.CostComponent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.ExileMaterials as ExileMaterials
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.RemovePlusOneCounters as RemovePlusOneCounters
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents

-- | One component of a Pawl.Types.Cost's non-mana part, alongside its mana part
-- (CR 601.2f).
--
-- Open-half card data. Pawl.Engine.Cost is where every reading of it lives but
-- one: CR 605.1a's "its cost and effect don't move any card to or from a
-- library" is Pawl.Engine.ManaAbility.costMovesLibraryCard, which cannot route
-- through Pawl.Engine.Cost without a module cycle (see that function).
-- Pawl.Engine.Filter.rewriteComponent cases on it too and reads nothing -- CR
-- 612.1's word swap is a traversal that reconstructs every arm.
--
-- Either way what the rules core takes from it is a CLASSIFICATION (can this be
-- paid? does it require the tap symbol? does it touch a library?) and never the
-- identity of a component.
--
-- PARAMETRIC in the keyword for the Filter it carries, and for that type's
-- reason alone -- see Pawl.Types.Filter.
data CostComponent keyword
  = -- | CR 107.5: tap this permanent; one already tapped can't pay the cost.
    -- CR 302.6 gates it on summoning sickness.
    TapThis
  | -- | CR 107.6: {Q}, untap this permanent; one already untapped can't pay the
    -- cost, and CR 302.6 gates it as it gates TapThis.
    UntapThis
  | -- | CR 701.21a / Mindslaver: sacrifice the object the cost is on.
    SacrificeThis
  | -- | CR 118.1 as a cost / Grinning Ignus: return the permanent the cost is on
    -- to its owner's hand.
    ReturnThis
  | -- | CR 119.4 / Greed: pay this much life, payable only out of a life total at
    -- least that large.
    PayLife Natural.Natural
  | -- | CR 107.3a / 601.2b / Hatred: X as an amount of life, announced by the
    -- caster and rewritten to a PayLife by Pawl.Engine.Cost.substituteX.
    PayLifeX
  | -- | CR 701.21a / Village Rites, Fireblast: sacrifice this many permanents
    -- matching the Filter, which the payer chooses.
    Sacrifice (Sacrifice.Sacrifice keyword)
  | -- | CR 702.122a's cost half / crew: tap any number of untapped permanents
    -- matching the Filter, chosen so that their TOTAL power reaches totalPower.
    TapForTotalPower (TapForTotalPower.TapForTotalPower keyword)
  | -- | CR 601.2f / Springleaf Drum: tap exactly this many permanents matching the
    -- Filter, chosen by the payer. CR 302.6 does not reach it -- see
    -- Pawl.Engine.Cost.requiresSicknessCheck.
    TapPermanents (TapPermanents.TapPermanents keyword)
  | -- | CR 118.1 as a cost / Meloku the Clouded Mirror: return exactly this many
    -- permanents matching the Filter to their owners' hands, chosen by the payer.
    ReturnPermanents (ReturnPermanents.ReturnPermanents keyword)
  | -- | CR 601.2f / 701.9b / Cathartic Reunion, Magmatic Insight: discard this many
    -- cards matching the Filter from hand, which the discarding player chooses.
    DiscardCards (DiscardCards.DiscardCards keyword)
  | -- | CR 702.29a / 702.77a / Faerie Macabre: discard the card the cost is on,
    -- carrying the DiscardCause the payment logs, since CR 702.29c's cycling
    -- trigger reads which rule spelled the cost.
    DiscardThis DiscardCause.DiscardCause
  | -- | CR 118.12 in its hand-to-battlefield form / Hakbal of the Surging Soul: the
    -- paying player puts one card matching the Filter from their own hand onto the
    -- battlefield.
    PutCardFromHandOntoBattlefield (Filter.Filter keyword)
  | -- | CR 107.14 / Longtusk Cub: pay this many energy counters.
    PayEnergy Natural.Natural
  | -- | CR 107.3a / 602.2b / Sphinx of the Revelation: X as an amount of energy,
    -- announced by the activating player.
    PayEnergyX
  | -- | CR 606.4 / Jace Beleren: put this many loyalty counters on the permanent
    -- the cost is on.
    AddLoyaltyToThis Natural.Natural
  | -- | CR 606.4's other half / Jace Beleren: remove this many loyalty counters
    -- from the permanent the cost is on, which CR 606.6 gates on it having them.
    RemoveLoyaltyFromThis Natural.Natural
  | -- | CR 118.1 as a cost / Barkhide Troll: remove this many +1\/+1 counters from
    -- the permanent the cost is on.
    RemovePlusOneCountersFromThis Natural.Natural
  | -- | CR 118.1 as a cost / Zameck Guildmage: remove this many +1\/+1 counters
    -- from one permanent matching the Filter, which the payer chooses.
    RemovePlusOneCounters (RemovePlusOneCounters.RemovePlusOneCounters keyword)
  | -- | CR 118.12's counter-placing cost / CR 701.63a's endure, Fortress
    -- Kin-Guard: put this many +1\/+1 counters on the permanent the cost is on,
    -- paid as the spell or ability resolves.
    PutPlusOneCountersOnThis Natural.Natural
  | -- | CR 701.68a as a cost / Bogslither's Embrace, Dawnhand Dissident: the paying
    -- player puts N -1\/-1 counters on a creature they control, and can't pay at
    -- all where they control none (CR 701.68b).
    Blight Natural.Natural
  | -- | CR 701.61a as a cost / Thornvault Forager, Camellia, the Seedmiser: the
    -- paying player forages, and can't pay at all where neither half of rule
    -- 701.61a can be carried out (CR 608.2d, Pawl.Engine.Forage.canForage).
    --
    -- Nullary, rule 701.61a fixing everything but the forager's own two choices --
    -- Pawl.Types.Effect's Forage arm is the same instruction from the other
    -- provenance, and Pawl.Engine.Forage.forage is the one procedure both reach.
    Forage
  | -- | CR 705.1 as a cost / Karplusan Minotaur's "Cumulative upkeep--Flip a
    -- coin": the paying player flips one coin of CR 705.2's win/lose kind, and
    -- can always pay -- a coin is not one of CR 118.3's resources, so no board
    -- lacks it.
    --
    -- The OUTCOME is not part of the payment. Rule 705.2's win or loss is a
    -- property of the flip that other abilities watch (TriggerCondition's
    -- PlayerWinsCoinFlip and PlayerLosesCoinFlip), not a condition on whether the
    -- cost was paid; a lost flip pays the cost exactly as a won one does.
    --
    -- Nullary, rule 705.1 fixing everything but the flipper's call.
    -- Pawl.Types.Effect's FlipCoin arm is the same instruction from the other
    -- provenance and binds a tally for a later effect to read, where a cost binds
    -- nothing; Pawl.Engine.Event.flipWinLoseCoin is the one procedure both reach.
    FlipCoin
  | -- | CR 107.3a / 601.2b / Soul Immolation: X as a blight amount, announced by
    -- the caster and rewritten to a Blight by Pawl.Engine.Cost.substituteX. The
    -- printed ceiling on X rides Pawl.Types.Face.maximumX (CR 101.1).
    BlightX
  | -- | CR 406.2 as a cost / Loxodon Surveyor: exile the card the cost is on, from
    -- the graveyard it is in -- the zone CR 113.6m then functions the ability in,
    -- read off the constructor by Pawl.Engine.Cost.zoneFunctionedFrom.
    ExileThisFromGraveyard
  | -- | CR 406.2 as a cost, from the other zone / Brittle Effigy: exile the
    -- permanent the cost is on, off the battlefield.
    ExileThis
  | -- | CR 406.2 in its choosing form / Headless Skaab: exile this many cards
    -- matching the Filter from the paying player's own graveyard (CR 400.3, CR
    -- 108.4), which the payer chooses.
    ExileCardsFromGraveyard (ExileCardsFromGraveyard.ExileCardsFromGraveyard keyword)
  | -- | CR 702.167a's [materials] / Tithing Blade: exile this many objects --
    -- or, where the payload says so, at least this many -- matching the Filter
    -- from among the permanents the paying player controls and the cards in their
    -- own graveyard, which the payer chooses.
    --
    -- A TWO-ZONE choice, Behold's shape below one zone over, and that is why this
    -- is not ExileCardsFromGraveyard with a wider criterion: rule 702.167b makes
    -- ONE criterion read over the battlefield and the graveyard at once, an
    -- exception to rule 109.2. Pawl.Engine.Cost.materialCandidates is the union
    -- and Prompt.ChooseMaterials the ask.
    --
    -- Binds nothing, unlike ExileThis above. Not implemented: CR 702.167c's "the
    -- exiled cards used to craft it", which is what would want the exiled
    -- materials bound here (#3931).
    ExileMaterials (ExileMaterials.ExileMaterials keyword)
  | -- | CR 406.2 in its fixed form / Circling Vultures: exile the topmost card of
    -- the paying player's graveyard matching the Filter, which CR 404.2's fixed
    -- order identifies without a prompt.
    ExileTopFromGraveyard (Filter.Filter keyword)
  | -- | CR 701.59a as a cost / Forensic Researcher: the paying player exiles any
    -- number of cards from their own graveyard whose total mana value reaches
    -- this number.
    --
    -- A THRESHOLD on an aggregate and not a count, TapForTotalPower's shape one
    -- zone over: how many cards a payment exiles is not settled until the payer
    -- picks them, which is why this is not ExileCardsFromGraveyard with a number.
    -- CR 701.59b is what makes it unpayable below the threshold.
    --
    -- BINDS the exiled cards under Pawl.Engine.Binding.collectedEvidence, which
    -- is what CR 701.59c's "if evidence was collected" is read off through
    -- Quantity.WasBound; CR 118.8b's optional "you may collect evidence" is a
    -- Pawl.Types.CostChoice, Behold's shape below.
    CollectEvidence Natural.Natural
  | -- | CR 406.2 out of a hidden zone / Cadaverous Bloom: the paying player
    -- exiles one card matching the Filter from their own hand, which the payer
    -- chooses.
    ExileCardFromHand (Filter.Filter keyword)
  | -- | CR 701.20a as a cost / Living Destiny: the paying player reveals one card
    -- matching the Filter from their own hand, which the payer chooses. CR 701.20b
    -- leaves the card in the hand, so this component spends nothing.
    RevealCardFromHand (Filter.Filter keyword)
  | -- | CR 701.4a as a cost / Caustic Exhale: the paying player beholds one object
    -- matching the Filter, either revealing a matching card from their hand or
    -- choosing a matching permanent they control.
    --
    -- A TWO-ZONE choice, which is why this is not RevealCardFromHand with a wider
    -- criterion: rule 701.4a offers the hidden hand OR the battlefield, and the
    -- payer picks which. Pawl.Engine.Cost.beholdCandidates is the union and
    -- Prompt.ChooseBehold the ask.
    --
    -- BINDS the beheld object under Pawl.Engine.Binding.beheldObject, which is
    -- what CR 701.4b's "if a [quality] was beheld" is read off through
    -- Quantity.WasBound; CR 118.8b's optional "you may behold" is an option of a
    -- Pawl.Types.CostChoice whose other option is empty.
    --
    -- Not implemented: beholding more than one object, and the Champion cycle's
    -- "behold a [quality] and exile it" -- the payload is one Filter and states
    -- neither (#3889).
    Behold (Filter.Filter keyword)
  | -- | CR 701.17a as a cost / Millikin: the paying player mills this many cards.
    -- The only component that moves a card out of a library, which CR 605.1a reads
    -- to bar a mana ability and CR 601.2h reads to put the payment in its second
    -- pass.
    MillCards Natural.Natural
  | -- | CR 702.174a as a cost / Scrapshooter: the paying player chooses an
    -- opponent, recorded on the object the cost is on as Object.chosenPlayer so
    -- that CR 400.7d's exception carries it to the permanent the spell becomes.
    --
    -- Nullary, FlipCoin's shape: rule 702.174a fixes every word of this cost but
    -- the payer's own call. Unpayable only where the payer has no opponent left
    -- (CR 102.2, CR 104.2a), which is what keeps it a cost rather than a no-op.
    ChooseOpponent
  | -- | CR 701.67a as a cost / Geyser Leaper: this much of the cost's generic
    -- mana is a waterbend cost, so the payer may tap an untapped artifact or
    -- creature they control rather than pay each of it.
    --
    -- A LICENCE and not a payment. The mana is in the cost's own mana part, so
    -- CR 601.2f's increases and reductions reach it as they reach every other
    -- generic symbol and CR 601.2g's window pays it; `payComponent` spends
    -- nothing for this arm, and `claimOf` states no claim -- the taps the payer
    -- substitutes arrive as a TapPermanents component of their own
    -- (Pawl.Engine.Cost.manaSubstitutions), which is what puts them on the
    -- ClaimAxis beside the rest of the cost.
    --
    -- The NUMBER is what CR 701.67b scopes the substitution to: the offer is
    -- capped at it even where the total cost holds other generic mana, so a
    -- waterbend {4} taxed {2} more by Suppression Field may tap four permanents
    -- and no more.
    --
    -- Not implemented: a waterbend cost in any other position -- a spell's
    -- additional cost, a ward cost, an unless cost, an alternative cost (#3901).
    Waterbend Natural.Natural
  | -- | CR 107.3a / 601.2b / Katara, Water Tribe's Hope: X as a waterbend
    -- amount, announced by the activator and rewritten to a Waterbend by
    -- Pawl.Engine.Cost.substituteX. BlightX's shape one keyword action over.
    --
    -- The MANA the licence scopes is the cost's own {X} (ManaSymbol.Variable),
    -- substituted by the same announcement, so this arm carries no number of its
    -- own: what it adds is CR 701.67b's ceiling, which without it would read 0
    -- and offer no tap at all.
    WaterbendX
  deriving (Eq, Ord, Show)
