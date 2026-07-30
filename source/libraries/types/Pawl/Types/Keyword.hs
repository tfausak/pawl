module Pawl.Types.Keyword where

import Numeric.Natural (Natural)
import Pawl.Types.Cost (Cost)
import Pawl.Types.Filter (Filter)

-- CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
-- comprehensive rules, the same as rule 506 or rule 302.
--
-- Casing on this is NOT a violation of the closed/open invariant. That invariant
-- forbids the rules core casing on the IDENTITY OF AN EFFECT; a keyword is a
-- numbered rule, so `case keyword of Flying -> ...` is the same kind of act as
-- casing on Phase. The test is "is it in the rulebook?" -- Flying is 702.9;
-- Goblin Piker is not in the rulebook. See the M2a spec, section 1, before
-- "fixing" this into a classification.
--
-- Constructors are ordered by RULE NUMBER, not by arrival, so this type stays
-- diffable against rule 702 itself. Each constructor below has a consumer --
-- P3a adds Fear (702.36, a colour-and-artifact blocking restriction) and
-- Devoid (702.114, a characteristic-defining ability that makes an object
-- colourless); P10 adds Infect (702.90, a deal-time damage-diversion bit).
--
-- Toxic (702.164) is the first PARAMETERIZED constructor, exactly the addition
-- the `data`-not-an-enum choice was made for; Poisonous (702.70) is the second,
-- and Landwalk Subtype (702.14), Protection Quality (702.16) and Ward Cost
-- (702.21) follow the same shape.
--
-- A keyword is not necessarily a STATIC ability: rule 702.70 spells poisonous
-- out as a TRIGGERED one. What it grants is still a citation and not an effect
-- identity, so Pawl.Keyword may read this constructor and mint rule 702.70a's
-- ability from it -- see that module.
--
-- Multiplicity is NOT this type's problem: an object can have the same keyword
-- ability twice, which Pawl.Types.ProjectedCharacteristics.keywords carries as a
-- count. This type says only WHICH ability, so a card's printed keywords stay a
-- Set (each printed once) -- see Pawl.Types.Card.keywords.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | Flying -- 702.9
  | Haste -- 702.10
  | Indestructible -- 702.12
  | Reach -- 702.17
  | Trample -- 702.19
  | Vigilance -- 702.20
  | -- 702.29a: "Cycling is an activated ability that functions only while the
    -- card with cycling is in a player's hand. 'Cycling [cost]' means '[Cost],
    -- Discard this card: Draw a card.'"
    --
    -- The cost rides the constructor, as Flashback's does, because rule 702.29a
    -- states it as part of the keyword rather than as separate card text.
    -- Pawl.Keyword.handAbilitiesOf mints the whole ability from this one value.
    --
    -- The "functions only while ... in a player's hand" half is NOT a field
    -- here. Rule 702.29b is explicit that the ability itself exists everywhere
    -- ("it continues to exist while the object is on the battlefield and in all
    -- other zones"), so the zone is a question the READER asks --
    -- Pawl.Activate.abilitiesFor -- exactly as Pawl.Cost.costsFor asks it of
    -- rule 702.34a's flashback cost.
    --
    -- Typecycling (702.29e) is this same ability with a library search in place
    -- of the draw: "'[Type]cycling [cost]' means '[Cost], Discard this card:
    -- Search your library for a [type] card, reveal it, and put it into your
    -- hand. Then shuffle your library.'" It rides THIS constructor rather than a
    -- sibling because CR 702.29f says every rule that looks for cycling finds it
    -- -- "typecycling abilities are cycling abilities, and typecycling costs are
    -- cycling costs" -- so one constructor makes that true for free instead of
    -- restating it at each reader.
    --
    -- Nothing is plain cycling and draws (702.29a); Just is what to search for. A
    -- Filter and not a Subtype, because rule 702.29e's "[type]" is not one: "this
    -- type is usually a subtype (as in 'mountaincycling') but can be any card
    -- type, subtype, supertype, or combination thereof (as in 'basic
    -- landcycling')" -- which is a Filter's whole job.
    Cycling Cost (Maybe Filter)
  | -- 702.34a: "Flashback appears on some instants and sorceries. It represents
    -- two static abilities: one that functions while the card is in a player's
    -- graveyard and another that functions while the card is on the stack.
    -- 'Flashback [cost]' means 'You may cast this card from your graveyard if
    -- the resulting spell is an instant or sorcery spell by paying [cost] rather
    -- than paying its mana cost' and 'If the flashback cost was paid, exile this
    -- card instead of putting it anywhere else any time it would leave the
    -- stack.'"
    --
    -- The cost rides the constructor, as Toxic's N does, because rule 702.34a
    -- states it as part of the keyword rather than as separate card text. It is
    -- deliberately NOT a Card.alternativeCosts entry: that list is
    -- unconditioned, so a flashback cost placed there would also be payable from
    -- the HAND. Pawl.Keyword turns this one value into all three of rule
    -- 702.34a's consequences -- the cost (Keyword.flashbackCost, read by
    -- Pawl.Cost.costsFor only in the graveyard), the permission
    -- (Keyword.castingPermissionsOf) and the exile replacement
    -- (Keyword.flashbackExile).
    Flashback Cost
  | Fear -- 702.36
  | -- 702.70a: "Poisonous is a triggered ability. 'Poisonous N' means 'Whenever
    -- this creature deals combat damage to a player, that player gets N poison
    -- counters.'" N rides the constructor, as Toxic's does. Unlike toxic, the
    -- N values are NOT summed: CR 702.70b says each instance triggers
    -- separately, so `Poisonous 1` twice is two abilities and two triggers --
    -- which is what Pawl.Keyword.triggeredAbilitiesOf builds from the
    -- projection's per-keyword count.
    Poisonous Natural
  | Infect -- 702.90
  | Devoid -- 702.114
  | -- 702.164a: "Toxic is a static ability. It is written 'toxic N,' where N is
    -- a number." N rides the constructor, so `Toxic 1` and `Toxic 2` are
    -- distinct keywords, and CR 702.164b's total toxic value is the sum over
    -- every toxic ability the creature has (Pawl.Projection.totalToxic) --
    -- including two with the same N, which the projection counts separately.
    Toxic Natural
  deriving (Eq, Ord, Show)

-- Devoid is read only at the projection SEED (Projection.baseColorsOf), not as a
-- layer-6 GainKeyword grant -- so a Devoid GRANTED by a layer-6 effect does
-- nothing to colour, silently. Expressible open-half data today; no card in the
-- pool does it (#35). See Projection.baseColorsOf's comment for the full
-- argument.
