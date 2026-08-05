module Pawl.Types.GameEvent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 608.2i: one entry of the turn-scoped record of what happened. Effects
-- that look back in time read it, so entries are APPENDED by the
-- change-and-emit funnels and never removed by a reader. Each reader keeps its
-- own watermark into GameState.events; the log itself is cleared only at turn
-- handoff.
data GameEvent
  = -- | CR 400.7: an object moved between zones. The ZoneChange is the RESOLVED
    -- (post-replacement) event, carrying the RESULTING object's id.
    --
    -- The ProjectedCharacteristics is the moved object as it last existed in the
    -- zone it LEFT (CR 608.2h). A snapshot, never a re-derivation from the
    -- printed card: a land animated into a creature DIED as a creature, and a
    -- token has no printed card at all (CR 111.3).
    --
    -- Strict (!): the snapshot is taken as of THIS zone change, not as of
    -- whenever a reader eventually forces it. An unforced field would be a thunk
    -- closing over the entire pre-move GameState, appended to a log that lives
    -- for a whole turn.
    Moved ZoneChange.ZoneChange !ProjectedCharacteristics.ProjectedCharacteristics
  | -- | CR 120 / 510: damage was dealt. The record the CR 704.5h deathtouch
    -- state-based action reads, watermarked rather than drained.
    DamageDealt DamageEvent.DamageEvent
  | -- | CR 615.13: a prevention effect was applied to one or more SIMULTANEOUS
    -- damage events and prevented some or all of that damage. The Recipient is
    -- whom the prevented damage would have been dealt to, and the Natural is how
    -- much of it was prevented -- always greater than 0, since a prevention that
    -- prevented nothing fires nothing.
    --
    -- ONE entry per prevention effect per batch, not per damage event: the rule
    -- says "one or more simultaneous damage events", so a shield spent across
    -- two attackers is a single prevention of the total.
    -- Pawl.Engine.Replacement.groupPreventions does that grouping; this record is
    -- what it turned into.
    --
    -- Not derivable from DamageDealt above, which is the whole reason this is its
    -- own constructor: CR 615.6 makes a fully prevented event never happen, so
    -- there is no DamageDealt to subtract from -- and even a partly prevented one
    -- records only what got through.
    --
    -- Carries no identity for the prevention EFFECT. The pool's one reader
    -- (Selfless Squire) triggers on any prevention at all, by its own ruling, and
    -- a card saying "prevented this way" is what must add one (#687).
    DamagePrevented Recipient.Recipient Natural.Natural
  | -- | CR 603.2b: a phase or step began, on whose turn (the active player). What
    -- both an "at the beginning of each end step" step trigger and a CR 603.7
    -- delayed ability match against.
    StepBegan Phase.Phase PlayerId.PlayerId
  | -- | CR 601.2i: a player cast a spell. The event Rule of Law counts, and the
    -- reason the count is a fold over the whole turn log rather than a per-effect
    -- watermark: its ruling looks at the entire turn, even if Rule of Law was not
    -- on the battlefield when the spell was cast. The CAST is the event, not the
    -- resolution -- a countered spell still counts.
    SpellCast PlayerId.PlayerId
  | -- | CR 725.1: a player became the monarch. What Palace Jailer's exile duration
    -- keys off, and the substrate for any future "whenever a player becomes the
    -- monarch" trigger.
    BecameMonarch PlayerId.PlayerId
  | -- | CR 701.9a: a card was DISCARDED. Emitted by Pawl.Engine.Event.discard,
    -- the one funnel every discard in the engine goes through, alongside the
    -- Moved event that same move records. The PlayerId is the discarding player,
    -- whom that rule makes the card's owner either way.
    --
    -- The ObjectId is the incarnation the card became, not the one that was in
    -- the hand: CR 400.7 mints a new object as it moves, and CR 702.29c's
    -- abilities trigger from whatever zone the card winds up in -- so the
    -- graveyard object is the one bearing the ability that triggers.
    --
    -- ONE event with two descriptions, which is the whole reason the cause is a
    -- FIELD rather than a sibling constructor. CR 702.29a makes cycling a
    -- discard, so a cycle has to be visible to both a "when you cycle this card"
    -- trigger (CR 702.29c) and a "whenever a player discards a card" one, with
    -- CR 702.29d capping a "cycles or discards" ability at one trigger per
    -- cycled card. A separate Cycled event
    -- appended beside this one would be a second record of a single discard,
    -- and any reader that matched both would answer twice.
    --
    -- Distinct from the Moved event the same discard also records: that one is
    -- the zone change (CR 400.7), and this one is what the change WAS. CR 701.9c
    -- is the rule that says they must stay apart -- a card a replacement sent to
    -- a hidden zone instead has still been discarded, so a reader matching the
    -- hand-to-graveyard zone pair would lose the case Rest in Peace creates.
    Discarded PlayerId.PlayerId ObjectId.ObjectId DiscardCause.DiscardCause
  | -- | CR 508.2b: an attacker was DECLARED -- one entry per creature the active
    -- player chose in CR 508.1's turn-based action. What "whenever this creature
    -- attacks" matches (CR 508.3a).
    --
    -- The declaration is the event, which is the whole point of recording it
    -- rather than reading Combat.attackers: CR 508.3a's last sentence excludes a
    -- creature put onto the battlefield attacking, and the combat record cannot
    -- tell the two apart. Only Pawl.Engine.Combat.declareAttackers appends this,
    -- and putOntoBattlefieldAttacking deliberately does not.
    AttackerDeclared ObjectId.ObjectId
  | -- | CR 701.20a: a player revealed a card.
    --
    -- A reveal is the one game action whose entire content is INFORMATION, so
    -- the log is where it has to live: CR 701.20b says revealing does not move
    -- the card, so an engine that did not record the reveal would be
    -- bit-for-bit identical to one that never performed it.
    --
    -- Carries the CARD's characteristics rather than an ObjectId, because what a
    -- reveal discloses is a card and not an identity -- and because the id is
    -- routinely dead by the time anything reads the event: every reveal in the
    -- pool today is a search's "reveal it, and put it into your hand", where
    -- CR 400.7 mints a new object one step later. An id joins this payload when
    -- a card needs to refer back to "that card" it revealed.
    --
    -- Strict (!) for GameEvent.Moved's reason.
    --
    -- This is the MOMENTARY reveal only. CR 701.20a's lasting cases -- a card
    -- revealed to pay a cost, and one that stays revealed while a triggered
    -- ability it caused is on the stack -- need a per-object flag that no card
    -- in the pool asks for (#185, #282).
    Revealed PlayerId.PlayerId !ProjectedCharacteristics.ProjectedCharacteristics
  | -- | CR 701.6a: a spell was COUNTERED. Emitted by Pawl.Engine.Event.counter,
    -- the one funnel every countering in the engine goes through, alongside the
    -- Moved event that same removal records.
    --
    -- Distinct from that Moved event, and the whole reason this constructor
    -- exists: CR 701.6a sends the countered spell to its owner's graveyard and
    -- CR 608.2n sends a spell that RESOLVED to the very same place, so a reader
    -- matching the stack-to-graveyard zone pair cannot tell the two apart -- and
    -- a discard or a mill lands a card in a graveyard too. What happened is not
    -- derivable from where the card went, so it is recorded.
    --
    -- Emitted ONLY where a countering actually happened. CR 113.6g's "can't be
    -- countered" functions on the stack and CR 101.2 makes the "can't" win, so a
    -- spell printing that clause is never countered at all -- Event.counter
    -- returns before this is recorded, which CR 603.2g makes mandatory.
    SpellCountered Countering.Countering
  | -- | CR 606.3: a LOYALTY ability of this permanent was activated -- the record
    -- that rule's once-per-permanent-per-turn limit is read out of.
    --
    -- A look-back read of the log rather than a stamp on the object, as
    -- Filter.AttackedThisTurn is. "That turn" then falls out for free: the log
    -- is cleared at the handoff, so the limit expires without anything having to
    -- reset it -- and a skipped untap step (CR 500.11) cannot strand it, which a
    -- per-turn flag cleared at untap could.
    --
    -- The ObjectId is the SOURCE PERMANENT, not the ability object CR 602.2a put
    -- on the stack, because CR 606.3 asks about that permanent. CR 400.7 mints a
    -- fresh id on every zone change, which is exactly right here -- a
    -- planeswalker that flickered is a new permanent and may activate again.
    --
    -- Recorded only for a LOYALTY ability, not for every activation: a permanent
    -- with both a loyalty ability and an ordinary one must not have the ordinary
    -- one count against CR 606.3, and no rule asks whether a permanent activated
    -- any ability this turn.
    LoyaltyAbilityActivated ObjectId.ObjectId
  deriving (Eq, Ord, Show)
