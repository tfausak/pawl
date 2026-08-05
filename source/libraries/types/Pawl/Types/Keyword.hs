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
-- Set -- see Pawl.Types.Face.keywords.
--
-- This module TIES THE KNOT that Pawl.Types.Filter's keyword parameter opens:
-- Filter has a HasKeyword arm and this type carries a Filter (702.11d, 702.14c,
-- 702.29e) and a Cost (702.29a/702.34a/702.42a) whose components carry one too, so the
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
  | -- | 702.11b with Nothing: this permanent can't be the target of spells or
    -- abilities your opponents control. 702.11d with Just: "hexproof from
    -- [quality]" -- this permanent can't be the target of [quality] spells your
    -- opponents control or abilities your opponents control from [quality]
    -- sources.
    --
    -- Shroud's sibling (702.18a) and deliberately NOT the same constructor: the
    -- CONTROLLER AXIS is what separates them, and both arms of this one carry it.
    -- Shroud names no player, so it stops the permanent's own controller as
    -- readily as anyone else; hexproof's "your opponents control" makes the answer
    -- depend on WHO is aiming the spell, which is why
    -- Pawl.Engine.Target.targetable reads CR 109.5's "you" and not only the
    -- candidate. The quality below narrows the same rule a second time, by WHAT is
    -- aiming -- the question protection (702.16b) asks and the one shroud never
    -- does.
    --
    -- The quality RIDES this constructor rather than taking one of its own,
    -- because rule 702.11d's last sentence says "a 'hexproof from [quality]'
    -- ability is a hexproof ability" and rule 702.11e spends three sentences
    -- spelling out what that buys: an effect that removes hexproof removes these
    -- too, an effect that ignores hexproof ignores these too, and an effect that
    -- looks for hexproof finds these too. One constructor makes all three true for
    -- free at every reader instead of restating them at each -- the argument CR
    -- 702.29f makes for typecycling riding Cycling. Humility (CR 613.1f) drops a
    -- "hexproof from black" because it drops KEYWORDS, not because anything
    -- enumerated the pair.
    --
    -- A Filter and not a Color, because rule 702.11d's "[quality]" is any quality:
    -- "hexproof from planeswalkers" (Eradicator Valkyrie) is a card type,
    -- "hexproof from instants" (Elenda, Saint of Dusk) another, and "hexproof from
    -- monocolored" (Sphinx of the Guildpact) is CR 105.2a's exactly-one-colour,
    -- which needs the combinators. Cycling's `Maybe (Filter Keyword)` exactly.
    --
    -- CR 702.11f's "hexproof from [quality A] and from [quality B]" and CR
    -- 702.11g's "hexproof from each [characteristic]" need no arm of their own:
    -- both rules say the card HAS SEVERAL hexproof abilities rather than one
    -- compound one, and Pawl.Types.Face.keywords is a Set, so such a card prints
    -- one entry per quality. CR 702.11h's redundancy is then per key, which is
    -- what its "the same hexproof ability" means.
    Hexproof (Maybe (Filter.Filter Keyword))
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
    -- Nullary, because rule 702.18a takes no parameter, and it asks neither of
    -- the two questions its neighbours do: not hexproof's "your opponents
    -- control" (702.11b/702.11d) about the targeting player, and not the stated
    -- quality of hexproof's variant (702.11d) or of protection (702.16a) about
    -- the source. That it asks neither is exactly why it is a separate keyword
    -- rather than another field on Hexproof above.
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
    -- Face.alternativeCosts entry: that list is unconditioned, so a flashback cost
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
    -- Face.additionalCosts entry: that list is unconditioned, so an entwine cost
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
-- is not built. No card in the pool grants devoid (#675).
