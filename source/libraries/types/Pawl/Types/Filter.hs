module Pawl.Types.Filter where

import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | A first-order predicate over one candidate -- an object, or (CR 115.1) a
-- player, since a target may be either -- expressed as data and evaluated by one
-- generic matcher (Pawl.Engine.Filter.matches) that never learns which effect
-- produced it. Its atoms case on CHARACTERISTICS, and on a handful of things CR
-- 109.3 says are NOT characteristics but that the closed half owns just as
-- squarely: combat status, attachment, what a permanent is represented by, and
-- what happened earlier this turn. Casing on a characteristic classification is
-- legitimate; the invariant forbids only casing on an EFFECT's identity.
--
-- Flat, not layered: the atoms and the And/Or/Not combinators are sibling arms of
-- one type, mirroring Pawl.Types.Quantity. A Simple/combinator split would buy an
-- enforceable normal form only if it also restricted the recursion (CNF/DNF).
-- `And []` is the trivial predicate, so a bare "target creature" needs no
-- separate "always" arm.
--
-- PARAMETRIC in the keyword, and only to break a module cycle: HasKeyword below
-- names Pawl.Types.Keyword, and that module already names this one (CR 702.29e
-- typecycling and CR 702.14c landwalk each carry their "[type]" as a Filter).
-- Pawl.Types.Keyword ties the knot by instantiating `Filter Keyword`, the single
-- application every module but this one, Pawl.Types.Cost and
-- Pawl.Types.CostComponent writes.
data Filter keyword
  = HasCardType CardType.CardType -- CR 205 / 300: the object's card types include this one.
  | HasSupertype Supertype.Supertype -- CR 205.4: the object's supertypes include this one.
  | HasColor Color.Color -- CR 105.2: the object's colours include this one.
  | HasSubtype Subtype.Subtype -- CR 205.3: the object's subtypes include this one.
  | -- | The object HAS this keyword ability (CR 702.1) -- Plummet's "target
    -- creature with flying" (CR 702.9). Needs no defence like the atoms below,
    -- since CR 109.3 lists abilities among an object's characteristics.
    --
    -- MEMBERSHIP, not equality of an ability list: the projection stores keywords
    -- as a count because CR 702 instances stack, and this asks only whether the
    -- key is present. For a parameterized keyword that makes `HasKeyword (Toxic
    -- 2)` ask about toxic 2 SPECIFICALLY, which is the narrow question and the
    -- right one: CR 702.14a's "[type]walk" is a written ability in its own right,
    -- so Quagmire's "creatures with swampwalk" must not reach islandwalk. Ask the
    -- family with HasKeywordFamily below; Pawl.FilterSpec pins the pair apart.
    --
    -- Read through the PROJECTION wherever one exists, so a creature that gains
    -- flying at CR 613.1f layer 6 matches and a Humility'd one stops matching;
    -- Projection.viewOfCard is the printed-card fallback off the battlefield.
    HasKeyword keyword
  | -- | The object has SOME keyword ability of this family (CR 702.1), whatever
    -- its payload -- Flensing Raptor's "another target creature you control with
    -- toxic", which a Phyrexian Mite with toxic 1 and a creature with toxic 3
    -- satisfy alike (CR 702.164a).
    --
    -- A SIBLING of HasKeyword above rather than a widening of it, because the two
    -- questions are both real: this one is what card text asks when it names the
    -- ability, and that one is what it asks when it names the written instance.
    -- Widening HasKeyword in place would have made `HasKeyword (Toxic 2)` and
    -- `HasKeyword (Toxic 3)` observably the same value.
    --
    -- CONCRETE where HasKeyword is parametric, which is why the parameter above
    -- survives: Pawl.Types.KeywordFamily is payload-free and imports nothing, so
    -- naming it here opens no cycle. Nullary keywords have no family constructor,
    -- so the two atoms partition rather than overlap -- there is exactly one way
    -- to write "a creature with flying" and exactly one to write "a creature with
    -- toxic". Answered off the PROJECTION for HasKeyword's reason.
    HasKeywordFamily KeywordFamily.KeywordFamily
  | PowerAtLeast Integer -- CR 208.1: the object's power is >= this literal.
  | -- | CR 208.1 in the other direction: the object's power is <= this literal --
    -- Ezuri, Claw of Progress' "a creature you control with power 2 or less".
    --
    -- A SIBLING of PowerAtLeast above and NOT its negation: power is a
    -- characteristic an object may simply not have, so `Not (PowerAtLeast 3)`
    -- admits every land, instant and player on the board, while this admits none
    -- of them. Both arms answer False for an absent power, so neither is
    -- reachable from the other, and Pawl.FilterSpec pins that pair apart.
    --
    -- Answered off the PROJECTION on the battlefield and off the printed power
    -- box everywhere else (Projection.printedPower), which is what Imperial
    -- Recruiter's "creature card with power 2 or less" reads as it searches a
    -- library.
    PowerAtMost Integer
  | -- | CR 202.3: the object's mana value is <= this literal -- Ojutai's
    -- Command's "creature card with mana value 2 or less".
    --
    -- Only AT MOST, where power above has both directions, because that is the
    -- direction the cards ask in: a mana value bounds what a cheap-thing effect
    -- may reach. Nothing in the pool asks for a mana value floor, and an unused
    -- arm is the speculative construction the project forbids.
    --
    -- Answerable OFF the battlefield, as the two power atoms above are and for
    -- the same reason: rule 202.3 reads the printed mana cost, which exists in
    -- every zone, and the graveyard is where the card asking is looking (CR 115.2's
    -- other zone half, via Pool.CardsInGraveyard). No Modification writes a mana
    -- cost, so there is nothing projected to read instead.
    ManaValueAtMost Integer
  | ControlledBy PlayerRelation.PlayerRelation -- CR 109.5 / 102.2: controller relates thus to the perspective.
  | -- | The candidate IS the evaluation's source object. Context-relative like
    -- ControlledBy: the Filter carries no object id, and the answer comes from the
    -- Context. `Not IsSource` is how CR 601.2c's "another" and a continuous
    -- effect's own "each other" card text (Opalescence) are both written -- one
    -- relation, one spelling, rather than a parallel Exclusion field on each
    -- (#163).
    IsSource
  | -- | CR 115.1: the candidate is a PLAYER who relates thus to the perspective --
    -- "target opponent". Context-relative like ControlledBy, but separate from it
    -- rather than a reuse, because ControlledBy asks who controls an OBJECT
    -- candidate and this asks who the candidate IS. CR 109.1's list of what an
    -- object is has no "player" in it, and CR 108.4 gives a controller only to a
    -- card representing a permanent or spell, so the two are never both answerable
    -- for one candidate.
    IsPlayer PlayerRelation.PlayerRelation
  | -- | CR 508.1k: the candidate is an ATTACKING creature -- declared as an
    -- attacker this combat phase and not since removed from combat (CR 506.4).
    -- Kill Shot's "target attacking creature".
    --
    -- The first atom reading something CR 109.3 says is NOT a characteristic,
    -- which is no breach of the invariant above: combat status is a RULES concept
    -- the closed half already owns (CR 506-511, Pawl.Types.Combat), so reading it
    -- is the same kind of act as reading a card type.
    IsAttacking
  | -- | CR 509.1g: the candidate is a BLOCKING creature -- declared as a blocker
    -- this combat phase and not since removed under CR 506.4. Labyrinth of
    -- Skophos' "target attacking or blocking creature" is spelled
    -- `Or [IsAttacking, IsBlocking]` rather than given a third atom meaning "in
    -- combat", the two roles being separate rules concepts separate cards ask
    -- about separately. Uncharacteristic for IsAttacking's reason.
    --
    -- NOT the same question as "is something blocking it": Pawl.Engine.Combat.isBlocked
    -- asks whether an ATTACKER has an entry in Combat.blockers, and this asks
    -- whether the candidate is a MEMBER of some attacker's set. CR 509.1h keeps
    -- them apart -- a creature remains blocked once every blocker leaves combat --
    -- so an attacker can be blocked when nothing answers True here.
    IsBlocking
  | -- | CR 608.2i: the candidate was DECLARED as an attacker earlier this turn --
    -- Relentless Assault's "all creatures that attacked this turn". A look-back
    -- read of the turn-scoped GameEvent log, which CR 608.2i sanctions; never a
    -- stamp on the object. Uncharacteristic for IsAttacking's reason.
    --
    -- NOT a synonym for IsAttacking, and not expressible in terms of it:
    -- Combat.attackers is wiped by Combat.clearCombat as the end of combat step
    -- ends (CR 511.3), so a postcombat main phase resolving this spell sees no
    -- attackers in the live record. The event log is the right footing because it
    -- is cleared at turn handoff, exactly the span "this turn" names.
    --
    -- DECLARED, like TriggerCondition.SelfAttacks and for that arm's reason: CR
    -- 508.4 says a creature put onto the battlefield attacking never attacked, and
    -- only Combat.declareAttackers appends GameEvent.AttackerDeclared.
    AttackedThisTurn
  | -- | CR 303.4b / 701.3a: the candidate is ATTACHED to a creature, which is what
    -- Crown of the Ages' "target Aura attached to a creature" narrows by.
    -- Uncharacteristic for IsAttacking's reason -- attachment is a rules concept
    -- the closed half owns (CR 301.5, 303.4, 701.3, Object.attachedTo).
    --
    -- Nullary and creature-specific rather than a recursive `AttachedTo Filter`:
    -- the narrowest atom the one card in the pool needs. The generalization is
    -- #356.
    IsAttachedToCreature
  | -- | CR 303.4 / 701.3a: the candidate is attached to a PERMANENT -- Aura Graft's
    -- "target Aura that's attached to a permanent". Strictly wider than
    -- IsAttachedToCreature above and strictly narrower than "attached to
    -- anything": CR 303.4 attaches an Aura to an object or a player, and only one
    -- of those is a permanent, so an enchant-player Aura is out.
    --
    -- Nullary for IsAttachedToCreature's reason and one more of its own. #356's
    -- general `AttachedTo Filter` needs a RECURSIVE Pawl.Engine.Filter.View -- a
    -- candidate's view carrying its host's -- which would make that record's
    -- derived Eq and Show diverge on a cyclic attachment. This atom needs neither:
    -- being attached to a permanent is a question about the ATTACHMENT, not about
    -- the host's characteristics, so it reads no second projection at all.
    IsAttachedToPermanent
  | -- | CR 701.3a's last sentence: the candidate is one the SUBJECT of the
    -- surrounding attach -- the permanent being moved -- could legally be attached
    -- to. Aura Graft's "another permanent IT CAN ENCHANT".
    --
    -- Context-relative like IsSource, except that the subject arrives through
    -- Pawl.Engine.Filter.View rather than through Context, the answer being
    -- per-candidate and needing the game state. Vacuously False wherever no attach
    -- frames the match.
    --
    -- Asks about the SUBJECT and not about the candidate, which no combination of
    -- the atoms above can express -- a destination filter narrowed by HasCardType
    -- would still admit a creature the Aura's enchant ability rejects. NOT a
    -- restatement of CR 303.4j, which is the backstop for a card that does NOT say
    -- "it can enchant" (Crown of the Ages), where the destination is offered and
    -- the move then fails.
    --
    -- Writing it into any other Filter position is a FAILING TEST rather than a
    -- quiet False: Pawl.CardSpec walks every Filter position a card has and
    -- rejects the atom in all but an attach's destination. Widening the subject so
    -- every evaluation could see it is #572. Reads the subject's enchant ability
    -- and not CR 303.4's other limits on what a permanent can be enchanted by
    -- (#472).
    CanHostSubject
  | -- | CR 111.6: the candidate is a token. Ashaya, Soul of the Wild's "nontoken
    -- creatures you control" is spelled `Not IsToken` -- one relation, one
    -- spelling, the way "another" is spelled `Not IsSource` (#163).
    -- Uncharacteristic for IsAttacking's reason (CR 111, Pawl.Types.Source).
    --
    -- Unlike the other such atoms it is not merely uncharacteristic but IMMUTABLE:
    -- CR 111.3 makes a token's effect-defined values equivalent to printed ones,
    -- so nothing in CR 613 can turn a card into a token or back. That is what lets
    -- Pawl.Engine.Projection.filterReads declare it as reading nothing.
    IsToken
  | -- | CR 110.5: the candidate is tapped. Wood Elemental's "untapped Forests"
    -- is spelled `Not IsTapped`, the one-relation-one-spelling posture IsToken's
    -- comment states (#163).
    --
    -- Uncharacteristic, like IsAttacking and IsToken: CR 110.5 makes tap state a
    -- STATUS rather than a characteristic, so nothing in CR 613 projects it and
    -- Pawl.Engine.Projection.filterReads declares it as reading nothing. Unlike
    -- IsToken it is not immutable -- a permanent taps and untaps constantly --
    -- but mutability is not what filterReads asks about.
    IsTapped
  | -- | CR 701.54e: the candidate "is your Ring-bearer" -- the permanent carrying
    -- the Ring-bearer designation made for the perspective player. The Ring's own
    -- "YOUR Ring-bearer is legendary" (CR 701.54c), which is rulebook text rather
    -- than a card's, so Pawl.Engine.Ring mints it.
    --
    -- Context-relative like IsSource and ControlledBy: the atom carries no
    -- PlayerId, and whose Ring-bearer is asked comes from the Context's
    -- perspective (CR 109.5). It needs no PlayerRelation payload either, unlike
    -- ControlledBy -- CR 701.54e is only ever asked as "YOUR Ring-bearer", and an
    -- opponent-relative spelling would be an arm no rule and no card asks for.
    --
    -- ONE of CR 701.54e's three conjuncts, not all three: this asks only about the
    -- designation, and the rule's "on the battlefield under your control" is
    -- spelled by the surrounding set -- Affected.Matching's own battlefield gate
    -- and a ControlledBy You conjunct beside this atom. Pawl.Engine.Ring's
    -- isRingBearerOf is the same three conjuncts assembled for a caller with no
    -- Filter in hand.
    --
    -- Uncharacteristic, for IsAttacking's reason: CR 701.54b makes Ring-bearer a
    -- DESIGNATION a permanent has, which CR 109.3's characteristic list has no
    -- room for and which the same rule says is not a copiable value. Like IsToken
    -- it is uncharacteristic AND unwritable by any CR 613 layer, which is what
    -- lets Pawl.Engine.Projection.filterReads declare it as reading nothing.
    IsRingBearer
  | And [Filter keyword]
  | Or [Filter keyword]
  | Not (Filter keyword)
  deriving (Eq, Ord, Show)
