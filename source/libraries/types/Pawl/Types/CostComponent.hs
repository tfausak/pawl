module Pawl.Types.CostComponent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Filter as Filter
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
  | -- | CR 118.12's counter-placing cost / CR 701.63a's endure, Fortress
    -- Kin-Guard: put this many +1\/+1 counters on the permanent the cost is on,
    -- paid as the spell or ability resolves.
    PutPlusOneCountersOnThis Natural.Natural
  | -- | CR 701.68a as a cost / Bogslither's Embrace, Dawnhand Dissident: the paying
    -- player puts N -1\/-1 counters on a creature they control, and can't pay at
    -- all where they control none (CR 701.68b).
    Blight Natural.Natural
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
  | -- | CR 406.2 in its fixed form / Circling Vultures: exile the topmost card of
    -- the paying player's graveyard matching the Filter, which CR 404.2's fixed
    -- order identifies without a prompt.
    ExileTopFromGraveyard (Filter.Filter keyword)
  | -- | CR 701.17a as a cost / Millikin: the paying player mills this many cards.
    -- The only component that moves a card out of a library, which CR 605.1a reads
    -- to bar a mana ability and CR 601.2h reads to put the payment in its second
    -- pass.
    MillCards Natural.Natural
  deriving (Eq, Ord, Show)
