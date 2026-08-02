module Pawl.Types.Keyword where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter

-- | CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
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
-- the `data`-not-an-enum choice was made for; Poisonous (702.70) is the second
-- and Landwalk (702.14) the third, and Protection Quality (702.16) and Ward Cost
-- (702.21) follow the same shape.
--
-- A keyword is not necessarily a STATIC ability: rule 702.70 spells poisonous
-- out as a TRIGGERED one. What it grants is still a citation and not an effect
-- identity, so Pawl.Engine.Keyword may read this constructor and mint rule 702.70a's
-- ability from it -- see that module.
--
-- Multiplicity is NOT this type's problem: an object can have the same keyword
-- ability twice, which Pawl.Types.ProjectedCharacteristics.keywords carries as a
-- count. This type says only WHICH ability, so a card's printed keywords stay a
-- Set (each printed once) -- see Pawl.Types.Card.keywords.
--
-- This module TIES THE KNOT that Pawl.Types.Filter's keyword parameter opens:
-- Filter has a HasKeyword arm (Plummet's "target creature with flying") and this
-- type carries a Filter (702.29e typecycling, 702.14c landwalk) and a Cost
-- (702.29a/702.34a/702.42a)
-- whose components carry one too, so the three of them would be a module cycle if
-- any were concrete. They are parametric and this one is not, which makes
-- `Filter Keyword` and `Cost Keyword` the only instantiations anywhere.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | -- | 702.8a: "Flash is a static ability that functions in any zone from which
    -- you could play the card it's on. 'Flash' means 'You may play this card any
    -- time you could cast an instant.'"
    --
    -- The first keyword here about WHEN a card may be cast. Rule 702's other
    -- casting keywords in this pool move a different axis: flashback (702.34a)
    -- moves the zone and the cost, cycling (702.29a) is an activated ability
    -- rather than a cast at all, and entwine (702.42a) moves the modes and the
    -- cost. This one moves only the window, and nothing reads it once the spell
    -- is on the stack. Read by Pawl.Engine.Cast.instantSpeed.
    --
    -- Nullary, because rule 702.8a takes no parameter, and CR 702.8b makes
    -- multiple instances redundant -- so its reader takes membership rather than
    -- the per-keyword count Pawl.Types.ProjectedCharacteristics.keywords carries.
    --
    -- Not a Pawl.Types.CastingPermission. That type's arms name a ZONE a card may
    -- be cast from (CR 601.3); rule 702.8a's second sentence names a TIME and no
    -- zone at all, and its first sentence is about where the ability functions
    -- rather than where the card may be cast from. A Pouncing Cheetah in a
    -- graveyard is as uncastable as a War Mammoth there.
    Flash
  | Flying -- 702.9
  | Haste -- 702.10
  | -- | 702.11a: "Hexproof is a static ability." 702.11b: "'Hexproof' on a
    -- permanent means 'This permanent can't be the target of spells or abilities
    -- your opponents control.'"
    --
    -- Shroud's sibling (702.18a) and deliberately NOT the same constructor: the
    -- CONTROLLER AXIS is the whole difference between them. Shroud names no
    -- player, so it stops the permanent's own controller as readily as anyone
    -- else; hexproof's "your opponents control" makes the answer depend on WHO
    -- is aiming the spell. That is why Pawl.Engine.Target.targetable reads CR
    -- 109.5's "you" -- the targeting player -- and not only the candidate.
    --
    -- Nullary, because rule 702.11b takes no parameter. Rule 702.11d's "hexproof
    -- from [quality]" is the parameterized variant and is not this constructor
    -- (#555): it reads the SOURCE's characteristics, which is protection's shape
    -- (702.16) rather than this one's.
    Hexproof
  | Indestructible -- 702.12
  | -- | 702.14a: "Landwalk is a generic term that appears within an object's rules
    -- text as '[type]walk,' where [type] is usually a land type, but it can also
    -- be the card type land plus any combination of land types, card types,
    -- and/or supertypes."
    --
    -- The qualification rides the constructor, as Cycling's Filter does, so
    -- `Landwalk (HasSubtype Swamp)` and `Landwalk (HasSubtype Island)` are
    -- distinct keywords. That is what CR 702.14d ("landwalk abilities don't
    -- 'cancel' one another") needs them to be: the reader looks up the DEFENDING
    -- PLAYER'S lands per landwalk walked, never at the blocker -- see
    -- Pawl.Engine.Combat.landwalkAllowsGiven.
    --
    -- A FILTER, not a Subtype, because CR 702.14c names four shapes and only the
    -- first is a bare land type -- "with the specified land type (as in
    -- 'islandwalk'), with the specified type or supertype (as in 'artifact
    -- landwalk'), WITHOUT the specified type or supertype (as in 'nonbasic
    -- landwalk'), or with both the specified type or supertype and the specified
    -- subtype (as in 'snow swampwalk')". The third needs a negation and the
    -- fourth a conjunction, neither of which a Subtype can say. Filter has both,
    -- so the four clauses are:
    --
    --   islandwalk         HasSubtype Island
    --   artifact landwalk  HasCardType Artifact
    --   nonbasic landwalk  Not (HasSupertype Basic)
    --   snow swampwalk     And [HasSupertype Snow, HasSubtype Swamp]
    --
    -- The filter carries the QUALIFICATION only, never the land-ness. Every
    -- clause of CR 702.14c is "at least one LAND with/without ...", so being a
    -- land is the rule's own conjunct rather than a parameter a card supplies,
    -- and landwalkAllowsGiven asks it separately -- which also keeps the CR
    -- 205.3d guard that comment describes. A card cannot forget it.
    --
    -- CR 702.14e ("multiple instances of the same kind of landwalk on the same
    -- creature are redundant") is why that reader takes membership rather than
    -- the per-keyword count Pawl.Types.ProjectedCharacteristics.keywords carries.
    -- "The same kind" is filter equality here, which is structural: two filters
    -- that mean the same thing written differently would count as two kinds, and
    -- redundancy would still give the same answer because the reader is an `any`.
    Landwalk (Filter.Filter Keyword)
  | -- | 702.15a: "Lifelink is a static ability." 702.15b: "Damage dealt by a
    -- source with lifelink causes that source's controller, or its owner if it
    -- has no controller, to gain that much life (in addition to any other
    -- results that damage causes)."
    --
    -- Nullary, because rule 702.15b takes no parameter, and CR 702.15f
    -- ("multiple instances of lifelink on the same object are redundant") is why
    -- its reader takes membership rather than the per-keyword count
    -- Pawl.Types.ProjectedCharacteristics.keywords carries -- flying's shape,
    -- not toxic's.
    --
    -- Read ONCE, at deal time, into Pawl.Types.DamageEvent.dealtByLifelink:
    -- CR 702.15c says the source's last known information decides whether it had
    -- lifelink, and CR 702.15d says the rules "function no matter what zone an
    -- object with lifelink deals damage from", so this is never a question asked
    -- of a live board when the life is handed over. See Pawl.Engine.Damage.damageEvent.
    Lifelink
  | Reach -- 702.17
  | -- | 702.18a: "Shroud is a static ability. 'Shroud' means 'This permanent or
    -- player can't be the target of spells or abilities.'"
    --
    -- The pool's first TARGETING RESTRICTION, read by the CR 115
    -- target-legality gate -- Pawl.Engine.Target.targetable, which is where
    -- every restriction rule 702 states lands. Hexproof (702.11b) is the second.
    --
    -- Nullary, because rule 702.18a takes no parameter. It is neither hexproof's
    -- "your opponents control" (702.11b) nor protection's stated quality
    -- (702.16a), and that those two ask about the targeting player and the
    -- source respectively is exactly why they are separate keywords rather than
    -- fields here.
    Shroud
  | Trample -- 702.19
  | Vigilance -- 702.20
  | -- | 702.29a: "Cycling is an activated ability that functions only while the
    -- card with cycling is in a player's hand. 'Cycling [cost]' means '[Cost],
    -- Discard this card: Draw a card.'"
    --
    -- The cost rides the constructor, as Flashback's does, because rule 702.29a
    -- states it as part of the keyword rather than as separate card text.
    -- Pawl.Engine.Keyword.handAbilitiesOf mints the whole ability from this one value.
    --
    -- The "functions only while ... in a player's hand" half is NOT a field
    -- here. Rule 702.29b is explicit that the ability itself exists everywhere
    -- ("it continues to exist while the object is on the battlefield and in all
    -- other zones"), so the zone is a question the READER asks --
    -- Pawl.Engine.Activate.abilitiesFor -- exactly as Pawl.Engine.Cost.costsFor asks it of
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
    Cycling (Cost.Cost Keyword) (Maybe (Filter.Filter Keyword))
  | -- | 702.34a: "Flashback appears on some instants and sorceries. It represents
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
    -- the HAND. Pawl.Engine.Keyword turns this one value into all three of rule
    -- 702.34a's consequences -- the cost (Keyword.flashbackCost, read by
    -- Pawl.Engine.Cost.costsFor only in the graveyard), the permission
    -- (Keyword.castingPermissionsOf) and the exile replacement
    -- (Keyword.flashbackExile).
    Flashback (Cost.Cost Keyword)
  | Fear -- 702.36
  | -- | 702.42a: "Entwine is a static ability of modal spells (see rule 700.2)
    -- that functions while the spell is on the stack. 'Entwine [cost]' means
    -- 'You may choose all modes of this spell instead of just the number
    -- specified. If you do, you pay an additional [cost].' Using the entwine
    -- ability follows the rules for choosing modes and paying additional costs
    -- in rules 601.2b and 601.2f-h."
    --
    -- The cost rides the constructor, as Flashback's and Cycling's do, because
    -- rule 702.42a states it as part of the keyword rather than as separate card
    -- text. It is NOT a Card.additionalCosts entry: that list is unconditioned,
    -- so an entwine cost placed there would be paid by every cast, and paying it
    -- is precisely what the player may decline (CR 601.2b's "announces their
    -- intentions to pay any or all of those costs").
    --
    -- The MODE-WIDENING half is not a field either. Rule 702.42a fixes it
    -- completely -- "all modes", never some other number -- so Pawl.Engine.Cast reads
    -- the payload's own mode count (Modal.modeCount) rather than a number
    -- restated here, and Pawl.Types.ModeSelection stays what the card PRINTS.
    Entwine (Cost.Cost Keyword)
  | -- | 702.70a: "Poisonous is a triggered ability. 'Poisonous N' means 'Whenever
    -- this creature deals combat damage to a player, that player gets N poison
    -- counters.'" N rides the constructor, as Toxic's does. Unlike toxic, the
    -- N values are NOT summed: CR 702.70b says each instance triggers
    -- separately, so `Poisonous 1` twice is two abilities and two triggers --
    -- which is what Pawl.Engine.Keyword.triggeredAbilitiesOf builds from the
    -- projection's per-keyword count.
    Poisonous Natural.Natural
  | Infect -- 702.90
  | -- | 702.111b: "A creature with menace can't be blocked except by two or more
    -- creatures."
    --
    -- Nullary like fear (702.36) and unlike landwalk, because 702.111b names no
    -- parameter -- the number two is written into the rule.
    --
    -- The first restriction of the SET shape #533 named, and its blocking half.
    -- Every other evasion ability
    -- here is a question about one (blocker, attacker) pair or less: flying asks
    -- about the blocker, fear asks about the blocker, landwalk asks about neither.
    -- Menace asks how MANY creatures are blocking, which no pairwise predicate can
    -- answer, so it is read by Pawl.Engine.Combat.menaceAllowsGiven -- a
    -- whole-declaration function -- rather than beside the other three in
    -- pairAllowedGiven.
    Menace
  | Devoid -- 702.114
  | -- | 702.164a: "Toxic is a static ability. It is written 'toxic N,' where N is
    -- a number." N rides the constructor, so `Toxic 1` and `Toxic 2` are
    -- distinct keywords, and CR 702.164b's total toxic value is the sum over
    -- every toxic ability the creature has (Pawl.Engine.Projection.totalToxic) --
    -- including two with the same N, which the projection counts separately.
    Toxic Natural.Natural
  deriving (Eq, Ord, Show)

-- Devoid is read only at the projection SEED (Projection.baseColorsOf), not as a
-- layer-6 GainKeyword grant -- so a Devoid GRANTED by a layer-6 effect does
-- nothing to colour, silently. Expressible open-half data today; no card in the
-- pool does it (#35). See Projection.baseColorsOf's comment for the full
-- argument.
