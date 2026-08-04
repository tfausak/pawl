module Pawl.Types.Keyword where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter

-- | CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
-- comprehensive rules, the same as rule 506 or rule 302. So casing on this is NOT
-- a violation of the closed/open invariant, which forbids the rules core casing
-- on the IDENTITY OF AN EFFECT. The test is "is it in the rulebook?" -- Flying is
-- 702.9; Goblin Piker is not. Constructors are ordered by RULE NUMBER, not by
-- arrival, so this type stays diffable against rule 702 itself.
--
-- A keyword is not necessarily a STATIC ability: rule 702.70 spells poisonous out
-- as a TRIGGERED one. What it grants is still a citation and not an effect
-- identity, so Pawl.Engine.Keyword may read a constructor and mint the rule's
-- ability from it.
--
-- Multiplicity is NOT this type's problem: an object can have the same keyword
-- ability twice, which Pawl.Types.ProjectedCharacteristics.keywords carries as a
-- count. This type says only WHICH ability, so a card's printed keywords stay a
-- Set -- see Pawl.Types.Card.keywords.
--
-- This module TIES THE KNOT that Pawl.Types.Filter's keyword parameter opens:
-- Filter has a HasKeyword arm and this type carries a Filter (702.29e, 702.14c)
-- and a Cost (702.29a/702.34a/702.42a) whose components carry one too, so the
-- three would be a module cycle if any were concrete. They are parametric and
-- this one is not, which makes `Filter Keyword` and `Cost Keyword` the only
-- instantiations anywhere.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | -- | 702.8a: this card may be played any time you could cast an instant. The
    -- only keyword here about WHEN a card may be cast -- rule 702's other casting
    -- keywords in this pool move a different axis -- and nothing reads it once the
    -- spell is on the stack. Read by Pawl.Engine.Cast.instantSpeed.
    --
    -- Nullary, because rule 702.8a takes no parameter, and CR 702.8b makes
    -- multiple instances redundant -- so its reader takes membership rather than
    -- the per-keyword count the projection carries.
    --
    -- Not a Pawl.Types.CastingPermission: that type's arms name a ZONE a card may
    -- be cast from (CR 601.3), where rule 702.8a names a TIME and no zone at all.
    -- A Pouncing Cheetah in a graveyard is as uncastable as a War Mammoth there.
    Flash
  | Flying -- 702.9
  | Haste -- 702.10
  | -- | 702.11b: this permanent can't be the target of spells or abilities your
    -- opponents control.
    --
    -- Shroud's sibling (702.18a) and deliberately NOT the same constructor: the
    -- CONTROLLER AXIS is the whole difference. Shroud names no player, so it stops
    -- the permanent's own controller as readily as anyone else; hexproof's "your
    -- opponents control" makes the answer depend on WHO is aiming the spell, which
    -- is why Pawl.Engine.Target.targetable reads CR 109.5's "you" and not only the
    -- candidate.
    --
    -- Nullary, because rule 702.11b takes no parameter. Rule 702.11d's "hexproof
    -- from [quality]" is the parameterized variant and is not this constructor
    -- (#555): it reads the SOURCE's characteristics, which is protection's shape
    -- (702.16).
    Hexproof
  | Indestructible -- 702.12
  | -- | 702.14a: "[type]walk", where the type is usually a land type but need not
    -- be. The qualification rides the constructor, as Cycling's Filter does, so
    -- `Landwalk (HasSubtype Swamp)` and `Landwalk (HasSubtype Island)` are
    -- distinct keywords -- which is what CR 702.14d needs, since landwalk
    -- abilities don't cancel one another: Pawl.Engine.Combat.landwalkAllowsGiven
    -- looks up the DEFENDING PLAYER'S lands per landwalk walked, never the
    -- blocker.
    --
    -- A FILTER, not a Subtype, because CR 702.14c names four shapes and only the
    -- first is a bare land type. The third needs a negation and the fourth a
    -- conjunction, neither of which a Subtype can say:
    --
    --   islandwalk         HasSubtype Island
    --   artifact landwalk  HasCardType Artifact
    --   nonbasic landwalk  Not (HasSupertype Basic)
    --   snow swampwalk     And [HasSupertype Snow, HasSubtype Swamp]
    --
    -- The filter carries the QUALIFICATION only, never the land-ness: every clause
    -- of CR 702.14c is about a LAND with or without something, so that conjunct is
    -- the rule's own and landwalkAllowsGiven asks it separately, which also keeps
    -- the CR 205.3d guard. A card cannot forget it.
    --
    -- CR 702.14e makes instances of the same kind redundant, which is why that
    -- reader takes membership rather than a count. "The same kind" is filter
    -- equality here, which is structural: two filters meaning the same thing
    -- written differently count as two kinds, and redundancy still gives the same
    -- answer because the reader is an `any`.
    Landwalk (Filter.Filter Keyword)
  | -- | 702.15b: damage dealt by a source with lifelink causes that source's
    -- controller (or owner) to gain that much life. Nullary, because rule 702.15b
    -- takes no parameter, and CR 702.15f makes multiple instances redundant -- so
    -- its reader takes membership rather than a count. Flying's shape, not
    -- toxic's.
    --
    -- Read ONCE, at deal time, into Pawl.Types.DamageEvent.dealtByLifelink: CR
    -- 702.15c decides lifelink-ness from the source's last known information and
    -- CR 702.15d makes it zone-independent, so this is never asked of a live board
    -- when the life is handed over.
    Lifelink
  | Reach -- 702.17
  | -- | 702.18a: this permanent or player can't be the target of spells or
    -- abilities. The pool's first TARGETING RESTRICTION, read by the CR 115
    -- target-legality gate (Pawl.Engine.Target.targetable), where every
    -- restriction rule 702 states lands.
    --
    -- Nullary, because rule 702.18a takes no parameter. It is neither hexproof's
    -- "your opponents control" (702.11b) nor protection's stated quality
    -- (702.16a), and that those two ask about the targeting player and the source
    -- respectively is exactly why they are separate keywords rather than fields
    -- here.
    Shroud
  | Trample -- 702.19
  | Vigilance -- 702.20
  | -- | 702.29a: pay the cost and discard this card to draw a card, functioning
    -- only while the card is in a player's hand. The cost rides the constructor,
    -- as Flashback's does, because rule 702.29a states it as part of the keyword
    -- rather than as separate card text; Pawl.Engine.Keyword.handAbilitiesOf mints
    -- the whole ability from this one value.
    --
    -- The hand-only half is NOT a field here: rule 702.29b is explicit that the
    -- ability exists in every zone, so the zone is a question the READER asks
    -- (Pawl.Engine.Activate.abilitiesFor), exactly as Pawl.Engine.Cost.costsFor
    -- asks it of rule 702.34a's flashback cost.
    --
    -- Typecycling (702.29e) is this same ability with a library search in place of
    -- the draw, riding THIS constructor rather than a sibling because CR 702.29f
    -- makes every rule that looks for cycling find it -- one constructor makes that
    -- true for free instead of restating it at each reader. Nothing is plain
    -- cycling; Just is what to search for, and a Filter rather than a Subtype
    -- because rule 702.29e's "[type]" may be any combination of card type, subtype
    -- and supertype.
    Cycling (Cost.Cost Keyword) (Maybe (Filter.Filter Keyword))
  | -- | 702.34a: this card may be cast from its owner's graveyard for the given
    -- cost, and is exiled instead of going anywhere else as it leaves the stack.
    --
    -- The cost rides the constructor, as Toxic's N does, because rule 702.34a
    -- states it as part of the keyword. It is deliberately NOT a
    -- Card.alternativeCosts entry: that list is unconditioned, so a flashback cost
    -- placed there would also be payable from the HAND. Pawl.Engine.Keyword turns
    -- this one value into all three of the rule's consequences -- the cost (read
    -- by Pawl.Engine.Cost.costsFor only in the graveyard), the permission and the
    -- exile replacement.
    Flashback (Cost.Cost Keyword)
  | Fear -- 702.36
  | -- | 702.42a: you may choose all modes of this modal spell (rule 700.2) instead
    -- of the number specified, paying an additional cost if you do.
    --
    -- The cost rides the constructor, as Flashback's and Cycling's do. It is NOT a
    -- Card.additionalCosts entry: that list is unconditioned, so an entwine cost
    -- placed there would be paid by every cast, and declining it is precisely the
    -- player's choice under CR 601.2b.
    --
    -- The MODE-WIDENING half is not a field either. Rule 702.42a fixes it
    -- completely -- all modes, never some other number -- so Pawl.Engine.Cast reads
    -- the payload's own Modal.modeCount rather than a number restated here, and
    -- Pawl.Types.ModeSelection stays what the card PRINTS.
    Entwine (Cost.Cost Keyword)
  | -- | 702.70a: whenever this creature deals combat damage to a player, that
    -- player gets N poison counters. N rides the constructor, as Toxic's does.
    -- Unlike toxic, the N values are NOT summed: CR 702.70b says each instance
    -- triggers separately, so `Poisonous 1` twice is two abilities and two
    -- triggers -- which is what Pawl.Engine.Keyword.triggeredAbilitiesOf builds
    -- from the projection's per-keyword count.
    Poisonous Natural.Natural
  | Infect -- 702.90
  | -- | 702.91a: whenever this creature attacks, each other attacking creature gets
    -- +1/+0 until end of turn. The SECOND keyword rule 702 states as a triggered
    -- ability, after poisonous (702.70a), and so the second one
    -- Pawl.Engine.Keyword MINTS rather than merely consults. Nullary, because rule
    -- 702.91a takes no parameter -- and unlike flying's or lifelink's nullary
    -- siblings, its reader takes the per-keyword count rather than membership,
    -- since CR 702.91b gives it the multiplicity CR 702.70b gives poisonous.
    BattleCry
  | -- | 702.111b: a creature with menace can't be blocked except by two or more
    -- creatures. Nullary like fear (702.36) and unlike landwalk -- the number two
    -- is written into the rule.
    --
    -- The first restriction of the SET shape #533 named, and its blocking half.
    -- Every other evasion ability here asks about one (blocker, attacker) pair or
    -- less; menace asks how MANY creatures are blocking, which no pairwise
    -- predicate can answer, so it is read by
    -- Pawl.Engine.Combat.menaceAllowsGiven -- a whole-declaration function --
    -- rather than beside the other three in pairAllowedGiven.
    Menace
  | Devoid -- 702.114
  | -- | 702.164a: toxic N. N rides the constructor, so `Toxic 1` and `Toxic 2` are
    -- distinct keywords, and CR 702.164b's total toxic value is the sum over every
    -- toxic ability the creature has (Pawl.Engine.Projection.totalToxic) --
    -- including two with the same N, which the projection counts separately.
    Toxic Natural.Natural
  deriving (Eq, Ord, Show)

-- Devoid is folded as a characteristic-defining ability at the start of layer 5
-- (Projection.applyColorDefining), per CR 613.3. A devoid GRANTED by a layer-6
-- effect still does nothing to colour: CR 604.3a(2) makes such a grant
-- non-characteristic-defining, so it would be an ordinary layer-5 effect, which
-- is not built. No card in the pool grants devoid (#622).
