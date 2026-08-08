module Pawl.Types.GameEvent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
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
  | -- | CR 601.2i: a player cast a spell -- the caster, and the spell. The event
    -- Rule of Law counts, and the reason the count is a fold over the whole turn
    -- log rather than a per-effect watermark: its ruling looks at the entire
    -- turn, even if Rule of Law was not on the battlefield when the spell was
    -- cast. The CAST is the event, not the resolution -- a countered spell still
    -- counts.
    --
    -- The ObjectId is the spell's STACK incarnation, the object CR 601.2a put
    -- there and the one CR 601.2i's "the spell becomes cast" is about. Not the
    -- card in the hand: CR 400.7 minted a new object on the way to the stack, and
    -- CR 601.2i applies the effects that modify the spell's characteristics as it
    -- is cast BEFORE it becomes cast -- so what a "whenever you cast a [type]
    -- spell" trigger asks about is already written on the stack object. Still on
    -- the stack when the trigger is checked, so CR 608.2h's last known
    -- information is not involved.
    --
    -- Not derivable from the Moved event that CR 601.2a's move to the stack
    -- records. Arriving on the stack is not being cast: CR 707.10 puts a COPY of
    -- a spell there and says in as many words that it "isn't cast", and CR 601.2
    -- can reverse a proposed cast after the move has already happened -- which is
    -- why Pawl.Engine.Cast emits this only past the last step that can fail.
    SpellCast PlayerId.PlayerId ObjectId.ObjectId
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
    -- Emitted ONLY where a countering actually happened. Both of Event.counter's
    -- "can't be countered" gates -- CR 113.6g's, printed on the spell, and CR
    -- 613.11's, handed out by a permanent's static ability -- return before this
    -- is recorded, since CR 101.2 makes either "can't" win and CR 603.2g makes
    -- the silence mandatory rather than tidy.
    SpellCountered Countering.Countering
  | -- | CR 119.3: a player LOST LIFE, and how much. Greater than 0 by
    -- construction: every producer guards its own zero. The rules state that
    -- explicitly only for the other direction -- CR 119.9's "if a player gains 0
    -- life, no life gain event has occurred" -- so reading it back onto loss is an
    -- inference, not a citation; what makes the guard safe is that both of this
    -- constructor's readers want a zero to be silent. CR 702.179d's speed
    -- increase must not fire on one, and neither must a card's "whenever an
    -- opponent loses life" (TriggerCondition.PlayerLosesLife), whose payload
    -- would otherwise read a 0 out of the amount slot.
    --
    -- Recorded at all three places life leaves a player, which is a fact about the
    -- RULES and not about the engine's plumbing: CR 119.3 for a loss an effect
    -- instructs (Pawl.Engine.Resolve's LoseLife arm), CR 119.2 for damage dealt to
    -- a player, which CAUSES life loss rather than being it (Pawl.Engine.Damage),
    -- and CR 119.4 for life paid as a cost (Pawl.Engine.Mana.payLife). A reader
    -- asking "did this player lose life" must find all three; anything narrower is
    -- a different question. CR 119.5's life-total set has no producer in the pool
    -- and so no site here.
    --
    -- LifeGained below is the sibling, and deliberately NOT one "life total
    -- changed" constructor covering both, though CR 119.3 does state the two
    -- directions in a single sentence: they are distinct EVENTS for triggers, and
    -- every card that cares says which.
    LifeLost PlayerId.PlayerId Natural.Natural
  | -- | CR 119.3's other direction: a player GAINED life, and how much. LifeLost
    -- above is the mirror, and the two are read by different cards.
    --
    -- Greater than 0 by construction, and here the rules SAY so rather than it
    -- being an inference: CR 119.9's "if a player gains 0 life, no life gain event
    -- has occurred, and these abilities won't trigger". Every producer guards its
    -- own zero, so a reader never has to.
    --
    -- Recorded at both places a source causes a player's life total to go up, which
    -- is a fact about the RULES and not about the engine's plumbing: CR 119.3 for a
    -- gain an effect instructs (Pawl.Engine.Resolve's GainLife arm), and CR 120.3f
    -- for lifelink damage, which gains its SOURCE'S CONTROLLER life rather than the
    -- damaged player (Pawl.Engine.Damage). A reader asking "did this player gain
    -- life" must find both.
    --
    -- Three life-total facts are deliberately NOT recorded here, each for a reason
    -- in the rules rather than an omission:
    --
    --   * A starting life total (CR 119.1) is not a gain. No source caused it, so
    --     CR 119.9's rewriting -- "whenever a source causes [a player] to gain
    --     life" -- has nothing to name.
    --   * Prevented damage (CR 615.6) is not a gain. The life was never lost, and a
    --     total that did not go DOWN did not go up; GameEvent.DamagePrevented is
    --     what records that, and CR 615.13 is its own separate trigger event.
    --   * Paying life (CR 119.4) only ever goes the other way -- that rule calls it
    --     losing life -- so the cost site records LifeLost and nothing here.
    --
    -- CR 119.5's life-total SET would record one, being a gain by that rule's own
    -- words whenever the new total is higher, but it has no producer in the pool
    -- and so no site here -- the same standing LifeLost's comment gives it.
    LifeGained PlayerId.PlayerId Natural.Natural
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
  | -- | CR 122.6: one or more counters were PUT onto an object -- the object, the
    -- kind, and the counts of that kind on it BEFORE and AFTER. Emitted by
    -- Pawl.Engine.Event.putCounters, the one placement funnel, and only once the
    -- CR 616.1 loop has settled how many of what kind actually land, so the pair
    -- describes the resolved event rather than the proposal.
    --
    -- The two counts rather than one amount, and the whole reason this constructor
    -- is shaped as it is: CR 714.2b's chapter ability asks whether the number "was
    -- less than N and became at least N", a THRESHOLD CROSSING that neither the
    -- amount alone nor the resulting total alone can answer. A Saga going from one
    -- lore counter to three crosses two thresholds at once, and one going from
    -- three to five crosses neither of those again.
    --
    -- BEFORE is strictly less than AFTER: putCounters returns early on a settled
    -- count of 0, so an event that placed nothing is never recorded. That matches
    -- CR 714.2b's "one or more" and keeps a would-be trigger from firing on a
    -- replacement that reduced the placement to nothing.
    --
    -- Removal is a record of its OWN -- CountersRemoved below -- rather than a
    -- before > after pair here. CR 122.6 is about putting counters on and every
    -- rule reading this constructor is phrased that way, so widening the pair
    -- would make every such reader ask which direction it went.
    CountersPut ObjectId.ObjectId CounterKind.CounterKind Natural.Natural Natural.Natural
  | -- | Counters were REMOVED from an object -- the object, the kind, and the
    -- counts of that kind on it BEFORE and AFTER. CountersPut's mirror, and shaped
    -- the same way for the same reason: CR 310.11b's Siege ability asks whether the
    -- LAST counter came off, which is a fact about the pair (before > 0, after ==
    -- 0) and not about the amount alone.
    --
    -- BEFORE is strictly greater than AFTER: a removal that took nothing is not
    -- recorded, which is CountersPut's "one or more" read the other way.
    --
    -- Recorded ONLY for CR 120.3h's damage to a battle (Pawl.Engine.Damage), which
    -- is the one removal a rule asks a trigger about. NOT IMPLEMENTED: the engine's
    -- other counter removals -- a loyalty cost, CR 306.8's damage to a
    -- planeswalker, CR 704.5q's annihilation -- stay direct writes and emit
    -- nothing, so a card triggering off one of those would not see it (#900).
    CountersRemoved ObjectId.ObjectId CounterKind.CounterKind Natural.Natural Natural.Natural
  | -- | CR 709.5c: a permanent was given an UNLOCKED DESIGNATION -- the permanent,
    -- and the half the designation names. Emitted by Pawl.Engine.Event.unlockHalf,
    -- the one place a designation is given, and only when the permanent did not
    -- already have it.
    --
    -- The event CR 709.5h asks about: "Some abilities trigger when a player
    -- unlocks a particular half of a permanent. These abilities trigger when that
    -- permanent is given the appropriate unlocked designation, regardless of
    -- whether it was given that designation while entering the battlefield or
    -- after entering the battlefield." That last clause is why this is an event
    -- and not something read off the board: the two moments leave the same board
    -- and only a record can tell them apart from a door that was already open.
    --
    -- The HALF by name, for the reason Object.unlockedHalves stores it by name:
    -- CR 709.4a makes a split card's halves the things a name picks out, and a
    -- Room with two "when you unlock this door" abilities needs each to know
    -- which door the event was about.
    --
    -- No player, though CR 709.5e's special action has one: the rule words the
    -- trigger as "when that permanent IS GIVEN the appropriate unlocked
    -- designation", and CR 709.5f's unlock reaches it with no payer at all. The
    -- ability's own controller is CR 603.3a's, read off the source as every other
    -- trigger's is.
    --
    -- NOT emitted for a LOCK (CR 709.5g): nothing in the pool locks a door, and no
    -- rule asks a trigger about one (#924).
    --
    -- The Bool is CR 709.5i's "fully unlocks": True when the permanent has ALL of
    -- its halves unlocked once this designation has been written. Carried on the
    -- event rather than re-derived when a trigger is matched, because by then the
    -- board has moved on -- a Room that left the battlefield, or whose other door
    -- was opened in the same settle, would answer a question about the present
    -- rather than about the moment the designation was given. Computed at the two
    -- write sites from one helper (Pawl.Engine.Event.fullyUnlockedAfter), so the
    -- entry designation (CR 709.5d) and the later one (CR 709.5f) cannot disagree
    -- about what "fully" means.
    HalfUnlocked ObjectId.ObjectId CardName.CardName Bool
  | -- | CR 708.7: a face-down permanent was turned face up. CR 708.8 makes that a
    -- change to one permanent's copiable values rather than a zone change, so no
    -- Moved event describes it and nothing else in this list carries it.
    --
    -- The PERMANENT by id and nothing else. CR 702.37e's special action turns
    -- over exactly one permanent, and the id is all any reading of the rule
    -- needs: the SELF-scoped condition compares it against the bearer, and the
    -- watcher-scoped form CR 708.7 also admits would filter on it (#959).
    --
    -- No PLAYER, for the reason HalfUnlocked carries none: CR 702.37e's action
    -- has a taker, but the trigger is worded about the permanent BEING turned
    -- face up, and CR 603.3a reads the ability's controller off its source.
    --
    -- One DIRECTION only. Turning a permanent face down (CR 708.2a's first
    -- sentence) is the opposite change and would be its own event -- and no
    -- printed card triggers on it, so there would be nothing to feed.
    TurnedFaceUp ObjectId.ObjectId
  deriving (Eq, Ord, Show)
