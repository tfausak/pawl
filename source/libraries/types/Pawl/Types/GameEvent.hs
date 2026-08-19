module Pawl.Types.GameEvent where

import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecameTarget as BecameTarget
import qualified Pawl.Types.BlockerDeclared as BlockerDeclared
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered

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
    Moved Moved.Moved
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
    DamagePrevented DamagePrevented.DamagePrevented
  | -- | CR 603.2b: a phase or step began, on whose turn (the active player). What
    -- both an "at the beginning of each end step" step trigger and a CR 603.7
    -- delayed ability match against.
    StepBegan StepBegan.StepBegan
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
    -- The ProjectedCharacteristics is that stack object as it was when the spell
    -- became cast (CR 608.2h), and it is what a LOOK-BACK reader needs rather
    -- than what a trigger needs: CR 608.2i's "for each spell you've cast this
    -- turn" is answered when the counting ability resolves, by which time the
    -- spell it names has resolved or been countered and its stack incarnation is
    -- gone. Recorded at CR 601.2i, after the effects that modify the spell as
    -- it's cast have been applied, so a cost-reduced or type-changed spell is
    -- counted as it was cast.
    --
    -- Strict (!) for Moved's reason above.
    --
    -- Not derivable from the Moved event that CR 601.2a's move to the stack
    -- records. Arriving on the stack is not being cast: CR 707.10 puts a COPY of
    -- a spell there and says in as many words that it "isn't cast", and CR 601.2
    -- can reverse a proposed cast after the move has already happened -- which is
    -- why Pawl.Engine.Cast emits this only past the last step that can fail.
    SpellCast SpellWasCast.SpellWasCast
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
    Discarded Discarded.Discarded
  | -- | CR 701.17a: a player MILLED cards. Emitted by Pawl.Engine.Resolve's Mill
    -- arm, the one place in the engine that mills, alongside the Moved event
    -- each of those moves records.
    --
    -- Distinct from those Moved events for the reason the Discarded arm above is
    -- distinct from its own, and the card's own ruling says so outright: a card
    -- put into a graveyard from a library without the word "mill" is not a legal
    -- target for The Master, Transcendent's ability. Surveil (CR 701.25a) and
    -- explore (CR 701.44a) both move a card from the top of a library into a
    -- graveyard without milling it, and a Moved entry records only the zone
    -- pair, so a reader folding those would admit all three.
    --
    -- ONE event per instruction per player, holding every card that player
    -- milled at once (CR 701.17a).
    Milled Milled.Milled
  | -- | CR 121.1: a player DREW a card, and which of that player's draws this
    -- turn it was -- 1 for the first, counting up. Emitted by
    -- Pawl.Engine.Event.drawCard, the one funnel every draw goes through.
    --
    -- The ORDINAL is on the event rather than left to a reader to fold out of the
    -- log, because CR 121.2 makes each draw its own event and CR 702.94a asks
    -- which one it was ("the first card you've drawn this turn"). The count it
    -- comes from is GameState.drawsThisTurn; recording it here is what keeps a
    -- reader of past history (CR 608.2i) agreeing with the live tally after the
    -- turn hands off and that tally is cleared.
    --
    -- Recorded only for a draw that COMPLETED. CR 121.4's attempt to draw from an
    -- empty library draws nothing -- it sets GameState.drewFromEmpty and loses the
    -- game at the next check -- so no card was drawn and nothing here says one
    -- was.
    --
    -- Distinct from the Moved event the same draw records, for CR 701.9a's reason
    -- one constructor up and CR 121.5's in as many words: a card an effect puts
    -- into a hand from a library without saying "draw" has not been drawn, so a
    -- reader matching the library-to-hand zone pair would answer for both.
    --
    -- The drawn CARD is deliberately not a field. No ability in the pool names it
    -- ("whenever you draw your second card each turn, put a +1\/+1 counter on this
    -- creature" points only at its own bearer), and the Moved event beside this
    -- one carries the object for anything that folds the log.
    Drew Drew.Drew
  | -- | CR 508.2b: an attacker was DECLARED -- one entry per creature the active
    -- player chose in CR 508.1's turn-based action. What "whenever this creature
    -- attacks" matches (CR 508.3a).
    --
    -- The declaration is the event, which is the whole point of recording it
    -- rather than reading Combat.attackers: CR 508.3a's last sentence excludes a
    -- creature put onto the battlefield attacking, and the combat record cannot
    -- tell the two apart. Only Pawl.Engine.Combat.declareAttackers appends this,
    -- and putOntoBattlefieldAttacking deliberately does not.
    --
    -- The PlayerId is CR 508.5's defending player FOR THIS CREATURE -- the
    -- player it is attacking, the controller of the planeswalker it is
    -- attacking, or the protector of the battle it is attacking -- computed by
    -- Pawl.Engine.Combat.declareAttackers as the declaration is written down.
    -- CR 702.86a's annihilator is what reads it.
    --
    -- Carried rather than derived, and CR 508.5 is itself the argument: that
    -- rule reads the defending player off what the creature is attacking, and
    -- both the planeswalker and the battle forms need the BOARD to answer.
    -- Pawl.Engine.Event.eventBindings takes no game state, so a derivation there
    -- is not available at all; and by the time a trigger resolves, the attacked
    -- planeswalker can be gone, which is the case CR 508.5's second sentence is
    -- about. Stamping the answer at declaration time is that rule's own moment.
    --
    -- CR 508.5a is why this is ONE player rather than a set: in a multiplayer
    -- game "defending player" means one specific defending player, determined
    -- individually per attacking creature -- which is exactly one field per
    -- declared attacker.
    --
    -- The Natural is HOW MANY creatures the SAME declaration named, which is CR
    -- 506.5's "the only creature declared as an attacker" -- 1 means the creature
    -- attacks alone, and CR 702.83a's exalted is what reads it. Never zero: a
    -- declaration naming nobody records no event. Every event from one
    -- declaration carries the same number, since the fact is about the
    -- declaration rather than about the creature.
    --
    -- Carried rather than derived, for BlocksDeclared's reason plus one of its
    -- own: CR 702.83b scopes "alone" to "a given combat phase", and the event log
    -- is cleared per TURN, so counting declarations in it would call a creature
    -- in the second combat phase of an extra-combat turn not alone.
    --
    -- NOT the AttackTarget itself. That is a wider payload for a different
    -- question -- CR 508.3a's attacks-a-permanent form, CR 508.3b and CR 508.3e,
    -- which need trigger conditions no card in the pool declares (#538).
    AttackerDeclared AttackerDeclared.AttackerDeclared
  | -- | CR 509.1i: a blocker was DECLARED -- one entry per creature the defending
    -- player chose in CR 509.1's turn-based action, naming the blocker and one
    -- attacking creature chosen for it (CR 509.1a). AttackerDeclared's mirror,
    -- and appended by Pawl.Engine.Combat.declareBlockers alone.
    --
    -- The declaration is the event, for AttackerDeclared's reason: CR 509.4 says
    -- a creature put onto the battlefield blocking is "blocking" but never
    -- "blocked", and Combat.blockers cannot tell the two apart.
    --
    -- ONE event per PAIR, which is CR 509.3b's arity ("once for each attacking
    -- creature the creature blocks"). CR 509.3a's once-per-blocker arity is
    -- BlocksDeclared below; the two differ exactly as BlockerDeclared and
    -- AttackerBlocked do on the attacking side.
    --
    -- The ATTACKER is the payload CR 509.3b's "blocks a creature" and CR 509.3d's
    -- "becomes blocked by a creature" need -- the first binds it, the second
    -- matches on it -- and it cannot be derived later, since a blocker removed
    -- from combat (CR 506.4) leaves no record of what it was declared against.
    BlockerDeclared BlockerDeclared.BlockerDeclared
  | -- | CR 509.1h: an attacking creature BECAME a blocked creature -- one event
    -- per attacker the CR 509.1 declaration gave at least one blocker, plus one
    -- per attacker an effect said becomes blocked (Effect.BecomesBlocked).
    -- Pawl.Engine.Combat.declareBlockers and Pawl.Engine.Combat.becomeBlocked are
    -- the two appenders, which are the two writers of the status itself.
    --
    -- Derived from the same declaration as BlockerDeclared and not folded into
    -- it, because the two have different arities: an attacker can be blocked by
    -- several creatures, so BlockerDeclared fires once per PAIR while CR 509.3c's
    -- "becomes blocked" fires once per attacker. Grouping is what makes the
    -- difference, and Pawl.Engine.Event.matchesTrigger sees one event at a time
    -- and so cannot do it.
    --
    -- The BLOCKERS are not carried. CR 509.3d's "becomes blocked by a creature"
    -- is the condition that names one, and it reads BlockerDeclared's pair
    -- instead -- this event exists to be the once-per-combat one (#1146).
    --
    -- CR 509.3c's third producer is missing: an attacker whose only blocker is
    -- one put onto the battlefield blocking becomes blocked too (CR 509.4 denies
    -- that creature having "blocked", but says nothing about the attacker), and
    -- nothing can put a creature onto the battlefield blocking (#1387).
    --
    -- The PlayerId is CR 508.5's defending player for this attacker, exactly as
    -- AttackerDeclared above carries it and computed the same way, off what the
    -- creature is attacking. CR
    -- 702.130a's afflict is what reads it. Carried rather than derived for
    -- AttackerDeclared's reason: Pawl.Engine.Event.eventBindings takes no game
    -- state, and the planeswalker and battle forms of CR 508.5 need the board.
    AttackerBlocked AttackerBlocked.AttackerBlocked
  | -- | CR 509.1h's other half: an attacking creature became an UNBLOCKED
    -- creature -- one event per attacker the CR 509.1 declaration gave no
    -- blockers, appended by Pawl.Engine.Combat.declareBlockers alone. The
    -- glossary's "attacks and isn't blocked" entry points at rule 509.1h for
    -- exactly this.
    --
    -- Recorded once, as the turn-based action finishes, and never again. That is
    -- rule 509.1h's own timing, and its last sentence is why nothing may sample
    -- Combat.blockers for the status later: a creature remains blocked even when
    -- every creature blocking it is removed from combat, so an entry that has
    -- emptied out is not this event.
    --
    -- The declaration is the only producer, and that is not a shortfall against
    -- rule 509.1h's "an effect says that it becomes blocked or unblocked": the
    -- blocked half of that clause is Pawl.Engine.Combat.becomeBlocked (Curtain
    -- of Light), and no printing states the unblocked half at all -- Scryfall
    -- has no card whose text makes a creature become unblocked. Nothing to
    -- observe, so nothing is elided.
    --
    -- No defending player rides it, unlike AttackerBlocked: the pool's reader
    -- (Eternal of Harsh Truths) names nobody but its controller. The printed
    -- forms that do say "defending player" -- Crypt Cobra's poison counter -- are
    -- not transcribed, and adding the field is the job of the card that reads it.
    AttackerUnblocked ObjectId.ObjectId
  | -- | CR 509.1i: a creature was declared BLOCKING -- one event per blocking
    -- creature the CR 509.1 declaration named, appended by
    -- Pawl.Engine.Combat.declareBlockers alone.
    --
    -- BlockerDeclared's grouped twin, and AttackerBlocked's mirror: that pair
    -- splits an attacker's declaration by CR 509.3c against CR 509.3d, this one
    -- splits a blocker's by CR 509.3a against CR 509.3b. Grouping is the whole
    -- difference, and Pawl.Engine.Event.matchesTrigger sees one event at a time
    -- and so cannot do it.
    --
    -- The Natural is HOW MANY attacking creatures it was declared against, which
    -- is CR 509.3e's number ("blocks two or more creatures"). Never zero: a
    -- creature that blocks nothing is not in the declaration. Carried rather than
    -- derived for AttackerBlocked's reason -- eventBindings takes no game state --
    -- and the attackers themselves are not, since the condition that names one
    -- reads BlockerDeclared's pair.
    --
    -- A DECLARATION is the only producer, which is STRICTER than rule 509.3a's
    -- second sentence: an effect that causes a creature to block also triggers
    -- it. No such effect is in the pool (#1146).
    BlocksDeclared BlocksDeclared.BlocksDeclared
  | -- | CR 701.20a: a player revealed a card.
    --
    -- A reveal is the one game action whose entire content is INFORMATION, so
    -- the log is where it has to live: CR 701.20b says revealing does not move
    -- the card, so an engine that did not record the reveal would be
    -- bit-for-bit identical to one that never performed it.
    --
    -- Carries the CARD's characteristics because what a reveal discloses is a
    -- card rather than an identity, and the ID BESIDE THEM because CR 702.94a
    -- needs to refer back to "this card": miracle's linked triggered ability
    -- (CR 603.11) is borne by the very card the reveal showed, still sitting in
    -- its owner's hand, so the snapshot alone could not say which object fired.
    -- The id is routinely dead by the time anything reads the event -- a search's
    -- "reveal it, and put it into your hand" moves the card one step later and
    -- CR 400.7 mints a new object -- so a reader that needs a live object must
    -- check, exactly as Discarded's does.
    --
    -- The RevealCause is CR 702.94a's "this way", and DiscardCause's shape one
    -- rule over: one showing of one card, described once, answering both "was a
    -- card revealed?" and "was it revealed as it was drawn?".
    --
    -- Strict (!) for GameEvent.Moved's reason.
    --
    -- This is the MOMENTARY reveal only. CR 701.20a's lasting cases -- a card
    -- revealed to pay a cost, and one that stays revealed while a triggered
    -- ability it caused is on the stack -- need a per-object flag that no card
    -- in the pool asks for (#185, #282).
    Revealed Revealed.Revealed
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
    -- and CR 119.4 for life a cost OR AN EFFECT has a player pay -- the cost side
    -- being a Phyrexian symbol or a PayLife component, the effect side an "as
    -- this permanent enters, you may pay N life" (both Pawl.Engine.Event.payLife,
    -- which is why rule 119.4 names them together). A reader
    -- asking "did this player lose life" must find all three; anything narrower is
    -- a different question. CR 119.5's life-total set has no producer in the pool
    -- and so no site here.
    --
    -- LifeGained below is the sibling, and deliberately NOT one "life total
    -- changed" constructor covering both, though CR 119.3 does state the two
    -- directions in a single sentence: they are distinct EVENTS for triggers, and
    -- every card that cares says which.
    LifeLost LifeChange.LifeChange
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
    LifeGained LifeChange.LifeChange
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
    CountersPut CounterChange.CounterChange
  | -- | Counters were REMOVED from an object -- the object, the kind, and the
    -- counts of that kind on it BEFORE and AFTER. CountersPut's mirror, and shaped
    -- the same way for the same reason: CR 310.12b's Siege ability asks whether the
    -- LAST counter came off, which is a fact about the pair (before > 0, after ==
    -- 0) and not about the amount alone.
    --
    -- BEFORE is strictly greater than AFTER: a removal that took nothing is not
    -- recorded, which is CountersPut's "one or more" read the other way.
    --
    -- Recorded by two paths: CR 120.3h's and CR 120.3c's damage to a battle or a
    -- planeswalker (Pawl.Engine.Damage, which diffs the boards rather than calling
    -- the funnel, its own comment saying why), and Pawl.Engine.Event.removeCounters
    -- -- the funnel Effect.RemoveCounters and CR 606.4's loyalty cost both go
    -- through. NOT IMPLEMENTED: CR 704.5q's annihilation of paired +1/+1 and -1/-1
    -- counters and CR 122.2's zone change stay direct writes and emit nothing, so a
    -- card triggering off one of those would not see it (#900).
    CountersRemoved CounterChange.CounterChange
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
    HalfUnlocked HalfUnlocked.HalfUnlocked
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
  | -- | A permanent GAINED THIS DESIGNATION -- CR 702.112b's renowned, CR 701.37b's
    -- monstrous or CR 701.60b's suspected. Emitted by Pawl.Engine.Resolve's
    -- Effect.Designate arm, the one place any of them is written, and only on a
    -- TRANSITION: a permanent already renowned does not become renowned again.
    -- HalfUnlocked's emission applies the same gate to its own designation.
    --
    -- The designation as a payload for Pawl.Types.Designation's reason, and one
    -- event rather than one per mark because one opcode writes them all.
    -- TriggerCondition.PermanentBecomesDesignated carries the same payload, so
    -- "when this creature becomes monstrous" (Arbor Colossus) reads this event
    -- without matching a permanent that became renowned.
    --
    -- The PERMANENT by id and nothing else, for TurnedFaceUp's reasons. No player:
    -- none of the rules that mint a designation names one, and CR 603.3a reads a
    -- watcher's controller off its own source.
    --
    -- One DIRECTION only. For renowned, monstrous and solved the rules make it
    -- the only one: "it stays renowned until it leaves the battlefield" leaves
    -- nothing to undo, and
    -- CR 400.7's new object is not a permanent losing a designation. CR 701.60a does
    -- let suspected end, and Effect.Unsuspect emits nothing -- no printed card
    -- triggers on a permanent ceasing to be suspected.
    BecameDesignated BecameDesignated.BecameDesignated
  | -- | CR 702.100b: a creature EVOLVED -- "one or more +1/+1 counters are put on
    -- it as a result of its evolve ability resolving". Emitted by
    -- Pawl.Engine.Resolve's Effect.Evolve arm, the one place that ability's
    -- counters are placed, and only when the placement actually landed some.
    --
    -- BecameDesignated's shape and its reasons: the permanent by id, no player, one
    -- direction. Unlike that one it marks no lasting designation -- rule 702.100b
    -- describes a moment rather than a state, so nothing on Pawl.Types.Object
    -- pairs with it and this event IS the whole record.
    --
    -- Distinct from the CountersPut event the same placement records: that one
    -- says +1/+1 counters arrived, this one says the evolve ability put them.
    -- Renegade Krasis reads the difference.
    Evolved ObjectId.ObjectId
  | -- | CR 702.134c: a creature MENTORED another -- "a mentor ability whose source
    -- is the first creature and whose target is the second creature resolves".
    -- Emitted by Pawl.Engine.Resolve's Effect.Mentor arm, the one place rule
    -- 702.134a's counter is placed.
    --
    -- TWO ids, in the rule's own order: the MENTOR, then the creature it mentored.
    -- The second is not derivable from the first, rule 702.134a's target being
    -- chosen (CR 603.3d), so only the resolution knows which creature it landed
    -- on; Pawl.Engine.Event.eventBindings hands it to the payload as
    -- Pawl.Engine.Binding.mentoredCreature, Aegis of the Legion's "that creature".
    --
    -- Emitted on the ability RESOLVING and gated on nothing else, which is where it
    -- parts company with Evolved above: rule 702.100b evolves a creature only if
    -- counters were actually put on it, and rule 702.134c asks only that the mentor
    -- ability resolve. A CR 122.6 replacement that reduces the placement to nothing
    -- still mentors. An ability that does NOT resolve emits nothing, which is CR
    -- 608.2b rather than a gate here: rule 702.134a's one target going illegal
    -- leaves nothing to resolve.
    --
    -- Distinct from the CountersPut event the same placement records, for Evolved's
    -- reason: that one says +1/+1 counters arrived, this one says a mentor ability
    -- put them and on whose say-so.
    Mentored Mentored.Mentored
  | -- | CR 702.149c: a creature TRAINED -- "a resolving training ability puts one
    -- or more +1/+1 counters on this creature". Emitted by Pawl.Engine.Resolve's
    -- Effect.Train arm, the one place rule 702.149a's counter is placed.
    --
    -- Evolved's shape and its reasons, one rule over: the creature by id, no
    -- player, one direction, and GATED on the placement having landed at least
    -- one counter -- rule 702.149c says "one or more" where rule 702.134c asks
    -- only that the ability resolve, so this sides with Evolved rather than with
    -- Mentored above.
    --
    -- ONE id and not Mentored's two: rule 702.149a puts its counter on the
    -- training creature itself, so the creature the trigger names is already
    -- Pawl.Engine.Binding.triggerSource and there is no second object to carry.
    --
    -- Distinct from the CountersPut event the same placement records, for
    -- Evolved's reason: that one says +1/+1 counters arrived, this one says a
    -- training ability put them. Savior of Ollenbock reads the difference.
    Trained ObjectId.ObjectId
  | -- | CR 701.21a: a permanent was SACRIFICED, and by whom. Emitted by
    -- Pawl.Engine.Event.sacrifice, the one funnel every sacrifice in the engine
    -- goes through -- a cost payment, Effect.Sacrifice, Effect.PlayerSacrifices
    -- and CR 704.5s's Saga rule all reach it.
    --
    -- Distinct from the Moved event the same sacrifice records, and this is the
    -- constructor's whole reason for existing: CR 700.4 makes "dies" mean "is put
    -- into a graveyard from the battlefield", so a sacrifice IS a death and its
    -- zone change is bit-for-bit the one a destruction or a mill writes. What
    -- happened is not derivable from where the permanent went, so it is recorded
    -- -- the argument GameEvent.Discarded and GameEvent.SpellCountered already
    -- make about their own moves.
    --
    -- CR 603.10a is why the record is written BEFORE the zone change, naming the
    -- PRE-MOVE id: "some zone-change triggers look back in time ... abilities that
    -- trigger when a player sacrifices a permanent". CR 701.21a's sacrifice is the
    -- game action, so a replacement that redirects the move (Rest in Peace) does
    -- not un-sacrifice the permanent, and an event recorded after the move would
    -- name an incarnation that a redirected move never produced.
    --
    -- Carries the SACRIFICING player, whom CR 701.21a makes the permanent's
    -- controller, and the permanent itself. Neither is read by a trigger today
    -- (Pawl.Engine.Event.eventBindingSlots answers empty for the condition), the
    -- payload being what a card printing "whenever YOU sacrifice" or "sacrifice a
    -- creature ... return IT" would need.
    PermanentSacrificed PermanentSacrificed.PermanentSacrificed
  | -- | CR 603.3b: an ABILITY TRIGGERED. The one entry in this log that describes
    -- something the rules did rather than something that happened to the board,
    -- and it is here because rule 603.3b names it as a trigger event in as many
    -- words: it splits the placement of a batch by whether an ability's "trigger
    -- condition ... isn't another ability triggering".
    --
    -- Appended by Pawl.Engine.Engine.placePendingTriggers, once per gathered
    -- trigger, BEFORE that batch is put onto the stack -- which is what lets the
    -- abilities reacting to it be gathered into the SAME batch and placed in rule
    -- 603.3b's second pass. Recording it after placement would put them in the
    -- next batch instead.
    --
    -- The ObjectId is the triggered ability's SOURCE (CR 113.7), which is how
    -- "the final chapter ability of a SAGA you control" finds the Saga. The
    -- PlayerId is the ability's CONTROLLER as it triggered (CR 603.3a), so
    -- "you control" is answered against the same sample every other part of the
    -- batch uses rather than against the board a later event may have changed.
    --
    -- The TriggerCondition says WHICH ability triggered. That is the honest
    -- identifier rather than a shortcut: CR 714.2 makes a chapter symbol "a
    -- keyword ability that represents a triggered ability", and CR 714.2b writes
    -- that ability's trigger condition out -- so a card asking whether a chapter
    -- ability triggered is asking about a condition. Nothing in the rules core
    -- reads the ability's EFFECT off this.
    --
    -- No entry for a SOURCELESS inherent ability (CR 725.2's monarch pair, CR
    -- 702.179d's speed increase, CR 728.1's rad counters) and none for a CR 603.7
    -- delayed ability, so nothing can trigger off one of those triggering (#1026).
    AbilityTriggered AbilityTriggered.AbilityTriggered
  | -- | A permanent's CONTROLLER CHANGED: the permanent, the player who controlled
    -- it when the game last looked, and the player who controls it now. The event
    -- Ray of Command's "when you lose control of the creature" matches (CR 603.7).
    --
    -- Control is DERIVED -- CR 613.1b puts it in layer 2, so the projection
    -- re-reads it live and no resolution announces the answer changing. This event
    -- is therefore SAMPLED into being, by Pawl.Engine.Engine.sampleControl
    -- comparing the live projection against GameState.controlSample, and is the
    -- one entry in this log minted from a difference between two boards rather
    -- than from an action. GameEvent.AbilityTriggered is the other engine-minted
    -- entry, and it is minted for the same reason: so the ordinary CR 603.2
    -- machinery can match something it otherwise could not see.
    --
    -- BOTH players, not just the new one. "When you LOSE control" asks about the
    -- player control left, which a one-player payload could not answer -- and the
    -- pair is also what makes the event self-evidently a change rather than a
    -- restatement, since sampleControl only mints one when they differ.
    --
    -- Battlefield-scoped, because that is where the sample is taken: a permanent
    -- LEAVING the battlefield loses no controller by this event's reckoning, even
    -- though its controller stops controlling it. CR 400.7 makes what comes back a
    -- new object with a new id, so nothing can observe the difference through a
    -- binding taken before the move.
    ControlChanged ControlChanged.ControlChanged
  | -- | CR 309.4c \/ 701.49a\/b: a player moved their venture marker into a room --
    -- the player, the dungeon card it is on, and which room. What CR 309.4c's
    -- unprinted trigger condition ("When you move your venture marker into this
    -- room") watches for.
    --
    -- Minted by Pawl.Engine.Dungeon.venture on ENTERING a dungeon as well as on
    -- advancing within one, because CR 701.49a puts the marker on the topmost room
    -- and CR 309.4c asks only that the marker MOVED INTO the room. The topmost
    -- room's ability triggers the first time a player ventures, which is the whole
    -- reason a first venture does anything.
    --
    -- Carries the dungeon's ObjectId as well as the room, because the room index
    -- alone names nothing: two players may be in room 1 of two different dungeons,
    -- and CR 309.4c makes each room ability the dungeon card's own.
    VentureMarkerEntered VentureMarkerEntered.VentureMarkerEntered
  | -- | CR 601.2c: an object or player BECAME A TARGET of a spell or ability --
    -- CR 115.1 makes a player a target in its own right, so the payload's
    -- `targeted` is a Recipient. Emitted by Pawl.Engine.Event.becameTarget, once
    -- per targeted recipient, as the targets are announced -- which is the moment that rule names, and the reason
    -- nothing here fires for an object merely CHOSEN at resolution (CR 115.10a
    -- makes such a choice no target at all).
    --
    -- Not derivable from GameEvent.SpellCast, which is why it is its own
    -- constructor: that event says a spell was cast and names none of its
    -- targets, an ACTIVATED ability records no cast event at all, and this is one
    -- event per target rather than one per announcement.
    BecameTarget BecameTarget.BecameTarget
  | -- | CR 701.3a: an Aura, Equipment or Fortification BECAME ATTACHED to an
    -- object or player -- rule 303.4b's "enchants" for an Aura. The payload's
    -- `host` is a Recipient for BecameTarget's reason, CR 702.5a's enchant
    -- ability being able to name a player.
    --
    -- Emitted by Pawl.Engine.Event.attach for rule 701.3's move, and by that
    -- module's zone-change funnel for a permanent that ARRIVES attached -- CR
    -- 608.3c's resolving Aura spell and CR 303.4f's entry choice. Two sites and
    -- one event, because the printings that read it ("whenever an Aura becomes
    -- attached to this creature") do not distinguish the routes.
    --
    -- NOT emitted by Pawl.Engine.Phasing, which is CR 702.26j: a permanent
    -- phasing in keeps the Object.attachedTo it phased out with, so there is no
    -- attachment for that path to record. Pawl.PhasingSpec holds the fence.
    --
    -- Not derivable from GameEvent.Moved: an attachment on the battlefield is no
    -- zone change at all, and an Aura that entered attached says nothing in its
    -- ZoneChange about what it landed on.
    BecameAttached BecameAttached.BecameAttached
  | -- | CR 800.4a: a permanent left the GAME, rather than the battlefield,
    -- because its owner left the game. The ObjectId is the id it had while it
    -- existed -- the key Pawl.Engine.Departure files its CR 608.2h last known
    -- information under, and the only route back to what it was, since leaving
    -- the game mints no new incarnation for it to become.
    --
    -- Not a GameEvent.Moved, and the difference is the rules': CR 800.4a takes
    -- the object out of the game entirely, so there is no destination zone to
    -- name and CR 400.7 never runs. A Moved carrying an invented destination
    -- would answer "did it go to a graveyard" -- which is what CR 700.4's
    -- "dies" asks -- and the answer would be a fiction.
    --
    -- Emitted for a PHASED-IN BATTLEFIELD permanent and for nothing else, which
    -- is exactly the set CR 603.6c's second trigger event ranges over: rule
    -- 702.26k says a phased-out permanent leaving this way causes no zone-change
    -- ability to trigger, and no rule reads the departure of a card that was in
    -- a hand, a library, a graveyard, exile or on the stack. Those still file
    -- last known information; what they do not do is enter this log.
    LeftTheGame ObjectId.ObjectId
  | -- | CR 701.22d: a player completed CR 701.22a's scry. Recorded AFTER the
    -- reorder, and recorded even where nothing could move -- that rule's "even
    -- if some or all of those actions were impossible" covers the empty library
    -- and the lone-card library Pawl.Engine.Resolve.scryOne puts no question
    -- for. CR 701.22b is the one case that is no scry at all, and Resolve's Scry
    -- arm returns on a quantity of zero before scryOne is reached.
    --
    -- Nothing else in the log says a scry happened: CR 701.20e's look mints no
    -- object and the reorder crosses no zone boundary, so this event has no
    -- Moved entry beside it to be confused with.
    Scried PlayerId.PlayerId
  | -- | CR 701.25d, Scried's twin, with CR 701.25c as its own non-event.
    --
    -- Distinct from the Moved entries the graveyard half of CR 701.25a records,
    -- and distinct from Milled for the reason that constructor's own comment
    -- gives -- a surveil moves a card from the top of a library into a graveyard
    -- WITHOUT milling it. A reader folding either would count cards binned
    -- rather than surveils performed, and would miss a surveil that binned
    -- nothing.
    Surveiled PlayerId.PlayerId
  | -- | CR 702.170a: a card became a plotted card. The ObjectId is the card AS
    -- IT LANDED IN EXILE -- Pawl.Engine.Plot.plot's `newId` and not the object
    -- that was in the hand -- because CR 400.7 mints a new object as it moves
    -- and that new one is what bears the ability CR 702.170e's "when this card
    -- becomes plotted" is printed on.
    --
    -- Distinct from the Moved entry the same exile records: a card exiled any
    -- other way makes the same hand-to-exile zone change without becoming
    -- plotted, so a reader matching the zone pair would admit both.
    --
    -- Not implemented: CR 702.170c's other route -- a spell or ability that
    -- makes a card in exile become plotted -- which would record this same
    -- event (#1390).
    Plotted ObjectId.ObjectId
  | -- | CR 701.44b: this permanent completed CR 701.44a's explore. Recorded
    -- after the whole process, and "even if some or all of those actions were
    -- impossible", so an explore off an empty library is still an explore.
    --
    -- The explorer's id ALONE, with no controller beside it. CR 701.44c makes
    -- last known information answer both halves of "which object explored and
    -- who controlled it", and a reader gets both from
    -- Pawl.Engine.Projection.viewWithLastKnown -- the reading the PermanentDies
    -- condition already gives a permanent that has left.
    --
    -- Distinct from the Revealed, Moved and CountersPut entries CR 701.44a's
    -- own steps record: each of those describes one step, any of them can be
    -- impossible, and none of them says an explore completed.
    Explored ObjectId.ObjectId
  | -- | CR 701.43a: this permanent was EXERTED. Recorded by
    -- Pawl.Engine.Combat.declareAttackers at CR 508.1g, the only place pawl
    -- exerts anything, and what CR 701.43d's linked "when you do" trigger
    -- watches for (TriggerCondition.SelfExerted). CR 701.43a states the keyword
    -- action generally, so an effect that exerted a permanent outside a
    -- declaration would record this same event.
    --
    -- The exerted permanent's id ALONE, Explored's shape and for its reason: at
    -- the moment recorded, the exerting player is the permanent's controller (CR
    -- 508.1a already required that of an attacker), and CR 603.2 matches the
    -- trigger against the event as it happens. The PROHIBITION outlives that
    -- moment and a control change can separate the two, so it records the player
    -- itself in Object.exertedBy rather than re-deriving one from this id.
    --
    -- Distinct from the AttackerDeclared event the same step records, and the
    -- distinction is the point: every attacker records that one, and CR 701.43d's
    -- trigger must fire only when the optional cost was actually PAID. It carries
    -- no "which declaration" tag either -- CR 701.43b lets a permanent be exerted
    -- more than once, so a second exert is a second event rather than a repeat.
    Exerted ObjectId.ObjectId
  deriving (Eq, Ord, Show)
