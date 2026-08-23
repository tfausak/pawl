module Pawl.Types.TriggerCondition where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency

-- | CR 603.2: the pattern that fires a triggered ability. Only
-- Pawl.Engine.Event may case on it for RULES purposes; Pawl.Codec also cases on
-- every constructor, but only as the JSON data boundary.
--
-- Two shapes recur. A Self- condition compares ids, so it reads none of the
-- subject's characteristics and never needs CR 608.2h last known information. A
-- filtered condition reads them, and so does. That is why several rules have a
-- constructor of each shape rather than one written as `Filter.IsSource`.
data TriggerCondition
  = -- | CR 603.6a: "when this ... enters". Self-scoped.
    SelfEnters
  | -- | CR 603.6a's second written form -- "whenever a [type] enters" -- fires
    -- whoever bears the ability, the bearer included, since CR 603.6a checks
    -- the newcomers too. Soul Warden's "another" is `Not IsSource` inside the
    -- Filter.
    PermanentEnters (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2b: "at the beginning of [each|your] <step>", against a
    -- GameEvent.StepBegan; the TurnScope decides whose turn qualifies. The
    -- ACTIVE player is bound under Pawl.Engine.Binding.triggerPlayer, which is
    -- "each player's upkeep ... that player" -- CR 109.5's "you" is the
    -- controller instead.
    StepBegins StepBegins.StepBegins
  | -- | CR 603.8: a STATE trigger -- it fires whenever its condition is true,
    -- and not again until the ability has left the stack, which is why
    -- Pawl.Engine.Event derives armedness from the stack rather than storing it.
    StateIs Condition.Condition
  | -- | CR 603.2 / 509-510: the bearer dealt combat damage to a player, off the
    -- DamageDealt history. Binds CR 702.70a's "that player" under
    -- Pawl.Engine.Binding.triggerPlayer and the amount that one event dealt
    -- under Pawl.Engine.Binding.eventAmount. The damager needs no slot, being
    -- the bearer.
    SelfDealsCombatDamageToPlayer
  | -- | CR 120.3: the bearer WAS DEALT damage -- the enrage trigger's event
    -- (Ripjaw Raptor). SelfDealsCombatDamageToPlayer's mirror off the same
    -- history, with the identity check on the recipient.
    --
    -- No DamageKind and no source Filter: the printed phrase qualifies the
    -- damage in no way. Enrage is an ability word (CR 207.2c), so nothing about
    -- it reaches Pawl.Types.Keyword. One fire per recorded event, not per batch,
    -- so a creature blocked by two triggers twice. Binds the amount under
    -- Pawl.Engine.Binding.eventAmount and CR 120.1's source under
    -- Pawl.Engine.Binding.combatDamager.
    SelfIsDealtDamage
  | -- | CR 603.2 / 509-510 read by a BYSTANDER: a permanent the Filter admits
    -- dealt combat damage to a player (Tovolar, Dire Overlord). Reads the
    -- damager's characteristics, so it goes through CR 608.2h for a trampler
    -- that died to its blocker in the same event.
    --
    -- Binds CR 510.2's damager under Pawl.Engine.Binding.combatDamager, the
    -- amount under Pawl.Engine.Binding.eventAmount, and the damaged player under
    -- Pawl.Engine.Binding.triggerPlayer.
    PermanentDealsCombatDamageToPlayer (Filter.Filter Keyword.Keyword)
  | -- | CR 725.2: a creature dealt combat damage to the monarch. Borne by no
    -- card; matched only via Pawl.Engine.Monarch.inherentMatch.
    CreatureDealtCombatDamageToMonarch
  | -- | CR 702.179d: "whenever one or more opponents lose life during your
    -- turn". The inherent speed-increase ability's event, and
    -- CreatureDealtCombatDamageToMonarch's sibling -- borne by no card, matched
    -- via Pawl.Engine.Speed.inherentPending, so
    -- Pawl.Engine.Event.matchesTrigger answers False for it.
    --
    -- The rule's other two clauses are elsewhere: "if your speed is less than 4"
    -- is CR 603.4's intervening "if" on Pawl.Types.TriggeredAbility.intervening,
    -- and "only once each turn" is that type's `limit`. Folding either in would
    -- make this constructor mean one ability instead of one event.
    OpponentLostLifeDuringYourTurn
  | -- | CR 702.29c: "when you cycle this card". Self-scoped. The bearer is the
    -- card in the zone it landed in, which is that rule's second sentence.
    SelfCycled
  | -- | CR 702.94a: "when you reveal this card this way" -- miracle's triggered
    -- half, linked (CR 603.11) to the static half. Self-scoped; CR 701.20b left
    -- the card in its owner's HAND, which is the candidate source CR 113.6k
    -- needs.
    --
    -- "THIS WAY" is the link: the event carries Pawl.Types.RevealCause, so a
    -- miracle card shown by any other effect does not fire it.
    --
    -- No PlayerRelation: the reveal is from the player's own hand and CR 113.8
    -- makes the owner the controller, so "you" and the revealer are one seat.
    SelfRevealedForMiracle
  | -- | CR 701.9a: "when you discard this card" (Bartered Cow). Self-scoped; CR
    -- 701.9a has already moved the card to its owner's graveyard, so the CR
    -- 400.7 incarnation is what the scan offers.
    --
    -- The CAUSE is deliberately not read: CR 702.29a makes cycling a discard, so
    -- a card discarded to a cycling cost fires this too, and CR 702.29d's once is
    -- supplied by the single GameEvent.Discarded funnel. No PlayerRelation,
    -- SelfRevealedForMiracle's argument one rule over.
    SelfDiscarded
  | -- | CR 701.9a: "whenever [a player] discards a card" (Megrim), against
    -- GameEvent.Discarded; the PlayerRelation reads the discarding player
    -- against CR 109.5's "you" (CR 603.3a). Not self-scoped: the bearer is a
    -- bystander.
    --
    -- The DiscardCause is deliberately not part of it, SelfDiscarded's argument.
    PlayerDiscards PlayerRelation.PlayerRelation
  | -- | "Whenever you cycle a card" (Prickly Marmoset). CR 702.29a makes cycling
    -- a discard, so this is PlayerDiscards narrowed to
    -- Pawl.Types.DiscardCause.ToPayCyclingCost; "you" is CR 603.3a's controller,
    -- read through the PlayerRelation. Not self-scoped, unlike SelfCycled: the
    -- card that left the hand is nothing to do with the bearer.
    --
    -- A separate constructor rather than a DiscardCause field on PlayerDiscards,
    -- because a card prints one phrase or the other and never both. CR 702.29d's
    -- "cycles or discards a card" needs neither, its event set being
    -- PlayerDiscards' exactly.
    PlayerCycles PlayerRelation.PlayerRelation
  | -- | CR 121.1: "whenever [a player] draws their Nth card each turn" (Erudite
    -- Wizard), against GameEvent.Drew, whose Natural is which of that player's
    -- draws this turn it was. PlayerDiscards' shape.
    --
    -- "Each turn" is not a field: the ordinal the event carries is already
    -- per-turn, GameState.drawsThisTurn being cleared at the handoff.
    --
    -- EQUALITY, not "at least": Erudite Wizard fires on the second draw and no
    -- other.
    PlayerDrawsNthCard PlayerDrawsNthCard.PlayerDrawsNthCard
  | -- | CR 508.3a: "whenever [a creature] attacks" (Hanweir Garrison).
    -- Self-scoped.
    --
    -- DECLARED is the whole content: CR 508.3a exempts a creature put onto the
    -- battlefield attacking and CR 508.4 says it never attacked, so this matches
    -- GameEvent.AttackerDeclared and never the combat record. No attack target is
    -- compared here; CR 508.5's defending player is bound under
    -- Pawl.Engine.Binding.triggerPlayer for CR 702.86a's annihilator.
    --
    -- The TriggerFrequency is Aurelia, the Warleader's "for the first time each
    -- turn" -- a payload rather than a sibling condition, since it narrows this
    -- same event.
    SelfAttacks TriggerFrequency.TriggerFrequency
  | -- | CR 508.3a with a companion required -- rule 702.149a's training.
    -- SelfAttacks' event and scoping, plus a Filter that is a predicate over
    -- everybody ELSE the same declaration named, asked existentially: one
    -- declaration is one event (CR 603.2), so it fires once however many
    -- companions qualify. Nothing is bound, rule 702.149a's payload naming only
    -- "this creature".
    --
    -- The companions come from the combat record and not the event log, which is
    -- what makes a second combat phase right -- the log keeps a turn's
    -- declarations, where Pawl.Types.Combat is cleared per combat phase. Reading
    -- the record is exact here despite CR 508.4, CR 508.2b putting these triggers
    -- on the stack before any player gets priority.
    SelfAttacksWithAnother (Filter.Filter Keyword.Keyword)
  | -- | CR 506.5: "whenever a creature you control attacks alone" -- rule
    -- 702.83a's exalted. SelfAttacks read by a BYSTANDER: an Aven Squire held
    -- back still triggers, so the bearer only frames the match, as
    -- PermanentEnters' does.
    --
    -- ALONE rides the constructor rather than the Filter, because CR 506.5 makes
    -- it a fact about the DECLARATION rather than a characteristic, and
    -- Pawl.Types.Filter's atoms are all characteristics. The number is read off
    -- the count GameEvent.AttackerDeclared carries, which is what makes rule
    -- 702.83b's "in a given combat phase" hold across an extra combat phase.
    --
    -- No TriggerFrequency: CR 506.1 gives a combat phase one declare attackers
    -- step. The attacker is bound under Pawl.Engine.Binding.attackingCreature.
    CreatureAttacksAlone (Filter.Filter Keyword.Keyword)
  | -- | CR 508.3a's second sentence read by a BYSTANDER: "whenever a creature
    -- attacks you or a planeswalker you control" (Marchesa's Decree).
    -- SelfAttacks' event and DECLARED reading.
    --
    -- Per declared attacker, so three attackers fire it three times. Nullary
    -- where CreatureAttacksAlone carries a Filter: CR 508.1a lets only a creature
    -- be declared, so "a creature" is no predicate. "You or a planeswalker you
    -- control" is ONE test, CR 508.5/508.5a making the defending player exactly
    -- the field the event carries, so this reads the event and never the board.
    -- The attacker is bound under Pawl.Engine.Binding.attackingCreature.
    --
    -- CR 508.3b's "[a player] is attacked" is the once-per-DECLARATION sibling,
    -- AttachedPlayerIsAttacked below, and two creatures attacking one player tell
    -- the two arities apart.
    CreatureAttacksYou
  | -- | CR 508.3b: "whenever enchanted player is attacked" (Curse of Vitality).
    -- CreatureAttacksYou's grouping sibling -- against
    -- GameEvent.BecameAttacked, which Pawl.Engine.Combat.declareAttackers
    -- records once per distinct target, so a declaration sending five creatures
    -- at one player fires this ONCE. That arity is the whole difference between
    -- the two arms, and it is structural rather than deduplicated: the condition
    -- sees one event at a time.
    --
    -- The ATTACHED player and not CR 109.5's "you", where CreatureAttacksYou
    -- takes the bearer's controller: the subject is whom the Aura enchants (CR
    -- 303.4m), read off Object.attachedTo as AttachedCreatureDies reads it. That
    -- is the only subject in print -- Scryfall o:"is attacked" o:"whenever",
    -- 2026-08-21, matches five cards and all five are Curses enchanting a player.
    --
    -- Nullary: rule 508.3b's subject is named by the ability's own attachment,
    -- so there is nothing left for a payload to say. The player is bound under
    -- the reserved Pawl.Engine.Binding.triggerPlayer slot, which the second half
    -- of every one of those five Curses needs ("each opponent attacking that
    -- player").
    --
    -- Not implemented: rule 508.3b's planeswalker and battle subjects -- the
    -- sweep above turned up no card writing one, and GameEvent.BecameAttacked
    -- already carries the permanent an arm for them would read -- and CR 508.3e's
    -- "[a player] attacks [another player]" (#538).
    AttachedPlayerIsAttacked
  | -- | CR 702.105a: dethrone -- SelfAttacks narrowed by whom the bearer
    -- attacked. The attacked player comes from Combat.attackers rather than the
    -- event, and that is the whole narrowing: the event carries CR 508.5's
    -- DEFENDING player, who may be a planeswalker's controller or a battle's
    -- protector, where the rule says "attacks THE PLAYER". Only
    -- AttackTarget.OfPlayer satisfies it.
    --
    -- Most life is compared across every player still in the game (CR 800.4a),
    -- the bearer's controller included. Read off the game state as the trigger is
    -- matched, not through Pawl.Types.Condition: an intervening "if" would be
    -- re-checked at resolution (CR 603.4, CR 608.2a), and rule 702.105a states
    -- none.
    SelfAttacksPlayerWithMostLife
  | -- | CR 509.3a: "whenever [a creature] blocks" (Pride Guardian). SelfAttacks'
    -- mirror, self-scoped, and DECLARED for its reason read on the blocking side
    -- (CR 509.4), so it matches GameEvent.BlockerDeclared and never
    -- Combat.blockers.
    --
    -- No TriggerFrequency: rule 509.3a's "only once each combat for that
    -- creature" is the GROUPING of GameEvent.BlocksDeclared, recorded once per
    -- blocking creature however many attackers it took. The attacker is neither
    -- compared nor bound.
    SelfBlocks
  | -- | CR 509.3b: "whenever [a creature] blocks a creature" (Loyal Sentry).
    -- SelfBlocks reading the per-pair GameEvent.BlockerDeclared instead, which
    -- is the rule's "once for each attacking creature ... blocks", and binding
    -- the attacker under Pawl.Engine.Binding.blockedCreature.
    --
    -- The Filter is a predicate over the ATTACKER blocked (Netcaster Spider's
    -- "with flying"), read at the scan, which is rule 509.3f's "at the point
    -- blockers are declared".
    --
    -- Not implemented: rule 509.3b's other producer, an effect that causes the
    -- bearer to block, records no event (#1146).
    SelfBlocksCreature (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e: "whenever [a creature] blocks two or more creatures"
    -- (Lairwatch Giant), against the grouped GameEvent.BlocksDeclared.
    --
    -- AT LEAST, never exactly, which is rule 509.3e's last sentence. The Natural
    -- is the floor, carried because the rule is written about a number.
    SelfBlocksAtLeast Natural.Natural
  | -- | CR 509.3e: "whenever [a creature] blocks one or more [F] creatures"
    -- (Serra Inquisitors' first half). SelfBlocksAtLeast with the number spent
    -- on a QUALITY instead, against the same grouped GameEvent.BlocksDeclared,
    -- so the bearer's whole declaration fires it once. CR 509.3b's per-attacker
    -- form is SelfBlocksCreature, and two admitted attackers tell them apart. No
    -- Natural: a floor beyond one would be a number no printing states.
    --
    -- The Filter is a predicate over each attacker blocked, read from
    -- Pawl.Types.Combat's declaration record rather than the event, which
    -- carries only how many.
    --
    -- Not implemented: rule 509.3e's "effects that add or remove blockers" reach
    -- neither this nor SelfBlocksAtLeast (#1146).
    SelfBlocksOneOrMore (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3c: "whenever [a creature] becomes blocked" (Sacred Prey). The
    -- ATTACKING side of SelfBlocks, against GameEvent.AttackerBlocked -- one per
    -- attacker that got at least one blocker, which is the rule's "only once
    -- each combat for that creature".
    --
    -- Rule 509.3c's second producer reaches it too: Effect.BecomesBlocked
    -- (Curtain of Light) records the same event, and the rule's "only if the
    -- attacking creature was an unblocked creature at that time" is
    -- Pawl.Engine.Combat.becomeBlocked's own guard. No blocker is bound; CR
    -- 508.5's defending player IS, which rule 702.130a's afflict reads.
    --
    -- The THIRD producer reaches it too: a creature put onto the battlefield as a
    -- blocker (CR 509.4), through
    -- Pawl.Engine.Combat.putOntoBattlefieldBlocking, which records the same
    -- event under the same unblocked-at-that-time guard. Flash Foliage is the
    -- pool's producer, and Pawl.CombatEffectSpec's PutOntoBattlefieldBlocking
    -- group is the proof.
    SelfBecomesBlocked
  | -- | CR 509.3d: "whenever [a creature] becomes blocked by a creature" -- rule
    -- 702.25a's flanking. Self-scoped on the attacking side, but against
    -- GameEvent.BlockerDeclared's PAIR rather than the grouped
    -- AttackerBlocked, the rule triggering "once for each creature that blocks".
    -- That arity is the whole difference from SelfBecomesBlocked.
    --
    -- The Filter is a predicate over the BLOCKER, read at the scan -- rule
    -- 509.3f's "at the point it becomes a blocking creature", CR 509.2a putting
    -- the triggers on the stack before any player gets priority. The blocker is
    -- bound under Pawl.Engine.Binding.blockingCreature.
    --
    -- Not implemented: rule 509.3d's other two producers, an effect that adds a
    -- blocker and a creature put onto the battlefield blocking, record no event
    -- (#1146).
    SelfBecomesBlockedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e: "whenever [a creature] becomes blocked by one or more [F]
    -- creatures" (Serra Inquisitors' second half) -- SelfBlocksOneOrMore from
    -- the ATTACKING side, against the grouped GameEvent.AttackerBlocked, so the
    -- whole declaration fires it once. That grouping is the whole difference
    -- from SelfBecomesBlockedBy.
    --
    -- No blocker is bound: the form names a SET, not an object.
    --
    -- Not implemented: rule 509.3e's other producers record no event (#1146).
    SelfBecomesBlockedByOneOrMore (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e read by a BYSTANDER on the ATTACKING side: "whenever a
    -- creature attacking one of your opponents becomes blocked by two or more
    -- creatures" (Seifer, Balamb Rival, the one printing -- Scryfall
    -- o:"becomes blocked by two or more", 2026-08-21). The arm above with the
    -- number spent on the blockers' COUNT rather than on a quality, and the
    -- subject moved off the bearer -- SelfBlocksAtLeast's floor read from the
    -- other side of the same declaration, so it matches the grouped
    -- GameEvent.AttackerBlocked and fires once for the attacker however many
    -- creatures blocked it.
    --
    -- AT LEAST, never exactly, which is rule 509.3e's last sentence.
    --
    -- The PlayerRelation is whom the attacker was declared attacking, and only
    -- Pawl.Types.AttackTarget.OfPlayer satisfies it: CR 508.1b lists player,
    -- planeswalker and battle separately, so a creature sent at a planeswalker
    -- an opponent controls is not attacking that opponent. That is why it is
    -- read from Pawl.Types.Combat's declaration record rather than from CR
    -- 508.5's defending player, which GameEvent.AttackerBlocked carries and
    -- which a planeswalker resolves to -- SelfAttacksPlayerWithMostLife makes
    -- the same distinction one rule over.
    --
    -- The attacker is bound under Pawl.Engine.Binding.attackingCreature, which
    -- is Seifer's "that attacking creature". No blocker is bound: the form
    -- names a number, not an object.
    --
    -- Not implemented: rule 509.3e's "effects that add or remove blockers",
    -- which record no event (#1146).
    CreatureBecomesBlockedByAtLeast CreatureBecomesBlockedByAtLeast.CreatureBecomesBlockedByAtLeast
  | -- | "Whenever this creature attacks and isn't blocked" -- Eternal of Harsh
    -- Truths', and CR 702.68a's frenzy, which Pawl.Engine.Keyword.frenzy mints.
    -- The glossary sends the phrase to CR 509.1h, so this matches
    -- GameEvent.AttackerUnblocked, recorded once per attacker the declaration
    -- left with no blockers -- the only arity the phrase can have, so no
    -- once-per-combat dedup is needed. Nothing is bound;
    -- GameEvent.AttackerUnblocked carries no player.
    --
    -- NOT "the bearer attacked and is not blocked right now": rule 509.1h fixes
    -- the status at the declaration and keeps it fixed, so a later state test
    -- would answer wrong for an attacker whose blockers all died.
    SelfAttacksUnblocked
  | -- | CR 603.6, a zone-change trigger: "when this card is put into your
    -- graveyard from your library" (Narcomoeba). Self-scoped.
    --
    -- The bearer is the card AS IT NOW IS IN THE GRAVEYARD -- CR 400.7's new
    -- incarnation, which CR 400.7e lets the ability find -- because CR 113.6k
    -- puts the ability wherever it can trigger from, and a card cannot be put
    -- into a graveyard from a library while it is on the battlefield. No zone
    -- pair is carried: the two zones are exactly what makes CR 113.6k apply.
    --
    -- The printed "your ... your" needs no controller check: a card goes to its
    -- OWNER's graveyard from its OWNER's library, and CR 113.8 makes that owner
    -- the ability's controller (CR 108.4).
    SelfPutIntoGraveyardFromLibrary
  | -- | CR 603.6: "when this card is put into a graveyard from anywhere" (Serra
    -- Avatar). Self-scoped, and the widest of the three put-into-a-graveyard
    -- conditions.
    --
    -- NOT a leaves-the-battlefield ability, which CR 603.6c's last sentence says
    -- in as many words, and which is why it is a sibling of SelfDies rather than
    -- its generalisation. Two consequences:
    --
    --   * no CR 603.10a look-back, so the bearer is the object as it exists
    --     AFTER the event -- the CR 400.7 graveyard incarnation, where SelfDies
    --     reads the departing one.
    --   * CR 113.6k puts the ability in the graveyard, which is what lets a
    --     Serra Avatar discarded out of a hand trigger at all.
    --
    -- A superset of SelfPutIntoGraveyardFromLibrary and still separate:
    -- Narcomoeba's sentence names one origin zone and must stay silent for a
    -- discard.
    SelfPutIntoGraveyardFromAnywhere
  | -- | CR 603.6c narrowed to CR 700.4's "dies", the battlefield-to-graveyard
    -- pair (Doomed Traveler). Self-scoped. CR 603.6c's wider first clause is
    -- SelfLeavesTheBattlefield.
    --
    -- The bearer is the incarnation that was ON THE BATTLEFIELD -- the id
    -- ZoneChange.departed carries and GameState.lastKnown files under -- which
    -- is CR 603.10a's look-back: the ability exists because the pre-move
    -- projection says so, and CR 603.3a's controller is whoever controlled the
    -- permanent as it left. The ARRIVING incarnation is bound separately under
    -- Pawl.Engine.Binding.became (CR 400.7e), so the bearer and the object the
    -- payload acts on are deliberately two different ids.
    SelfDies
  | -- | The SAME written form read by a BYSTANDER (Meren of Clan Nel Toth).
    -- Filtered rather than self-scoped, so it reads the dead permanent's
    -- characteristics; "another" is `Not IsSource` inside the Filter.
    --
    -- The candidate is ZoneChange.departed and NOT ZoneChange.object, read from
    -- CR 608.2h last known information (CR 603.10a). That is what makes "you
    -- control" answerable correctly: by the CR 117.5 boundary the candidate is a
    -- card in a graveyard, which CR 108.4 gives no controller and CR 108.4a
    -- would hand back its OWNER. What the PAYLOAD acts on is a different id, as
    -- for SelfDies: Pawl.Engine.Binding.became holds the graveyard incarnation,
    -- which Promise of Tomorrow's "exile it" reads.
    PermanentDies (Filter.Filter Keyword.Keyword)
  | -- | CR 603.6c's first written form taken whole -- "when [this object] leaves
    -- the battlefield", so an exile, a bounce and a shuffle into a library all
    -- fire it (Thragtusk). Self-scoped and a LOOK-BACK for SelfDies' reasons.
    --
    -- A sibling of SelfDies, never its superset in code even though it is one in
    -- the rules: a Doomed Traveler must stay silent for exactly the bounce that
    -- fires a Thragtusk. Where it diverges is CR 400.7e, whose rescue of the
    -- arriving object holds only for a public destination, so
    -- Pawl.Engine.Binding.became is bound only there.
    --
    -- CR 603.6c's second trigger event -- a phased-in permanent leaving the game
    -- with its owner (CR 800.4a) -- is matched too, off GameEvent.LeftTheGame.
    -- It is the one form that fires without a zone change, so its bearer can
    -- only be read from CR 608.2h last known information.
    SelfLeavesTheBattlefield
  | -- | The SAME written form read by a BYSTANDER (Super Shredder's "whenever
    -- another permanent leaves the battlefield"). SelfLeavesTheBattlefield's
    -- events -- any destination, plus CR 603.6c's leaving-the-game form -- with
    -- PermanentDies' scoping: filtered rather than self-scoped, so it reads the
    -- departing permanent's characteristics, and "another" is `Not IsSource`
    -- inside the Filter.
    --
    -- The candidate is ZoneChange.departed and NOT ZoneChange.object, read from
    -- CR 608.2h last known information (CR 603.10a) -- PermanentDies' argument,
    -- and stronger here: this condition's destination may be a hand or a library,
    -- where there is no public incarnation to read at all.
    --
    -- No card in `data/cards/` needs the arriving incarnation, so what the
    -- payload could act on is bound exactly as SelfLeavesTheBattlefield binds it
    -- -- only for a public destination (CR 400.7e), and never for the
    -- leaving-the-game form, which reaches no zone.
    PermanentLeavesTheBattlefield (Filter.Filter Keyword.Keyword)
  | -- | CR 700.4's "dies" read off the permanent the BEARER IS ATTACHED TO
    -- (Screams from Within's "when enchanted creature dies"). PermanentDies'
    -- battlefield-to-graveyard pair and a look-back for its reasons; WHICH
    -- permanent is Object.attachedTo, AttachedCreatureMentors' scoping.
    --
    -- Attachment-scoped rather than filtered, for the reason rule 702.134c's
    -- condition gives: CR 303.4b's "enchanted creature" is a link the Aura
    -- records, not a class a Filter could name.
    --
    -- The one condition CR 113.6m's Aura clause names, which is why
    -- Pawl.Engine.Event.zoneFunctionedFrom cases on it: the rule exempts an
    -- ability whose trigger condition "specifies that ... the object it enchants
    -- leaves the battlefield" from being pinned to the zone its effect moves the
    -- object out of.
    --
    -- Vacuously False while the source is attached to nothing, or to a player
    -- (CR 303.4), AttachedCreatureMentors again.
    --
    -- The link is read from CR 608.2h last known information where the bearer is
    -- already gone (Pawl.Types.LastKnown.attachedTo), which is the ordinary case
    -- rather than the exotic one: CR 704.5m buries the Aura in the same SBA batch
    -- that buried its host, and CR 117.5 places triggers only after that batch
    -- settles.
    --
    -- Not implemented: the payload finding the Aura in the graveyard, which is
    -- CR 400.7f rather than this rule (gap #1892).
    AttachedCreatureDies
  | -- | CR 702.55b/702.55c: "when the creature this card haunts dies", borne by
    -- the haunting CARD IN EXILE. PermanentDies' zone pair; WHICH permanent is
    -- the one GameState.haunting files the bearer against, the link
    -- Effect.ExileHaunting wrote.
    --
    -- The only condition that cannot trigger from the battlefield for a reason
    -- about the BEARER rather than the event: a permanent on the battlefield
    -- haunts nothing, so CR 113.6k sends this to exile. No Filter -- rule 702.55b
    -- makes the haunted object the one the haunt ability targeted "regardless of
    -- whether or not that object is still a creature".
    HauntedCreatureDies
  | -- | CR 701.6a: "whenever a spell or ability you control counters a spell"
    -- (Baral, Chief of Compliance), against GameEvent.SpellCountered; the
    -- relation is on the COUNTERING side, never the countered spell's.
    --
    -- Only a countered SPELL fires this: CR 113.9 says an ability on the stack is
    -- not a spell, so Pawl.Engine.Event.counter ceases it (CR 608.2n) and records
    -- no event.
    --
    -- Not implemented: a countered-ability event and its condition (#541).
    SpellOrAbilityCounters PlayerRelation.PlayerRelation
  | -- | CR 615.13: "whenever damage that would be dealt to you is prevented"
    -- (Selfless Squire), against GameEvent.DamagePrevented; the relation reads
    -- the recipient against CR 109.5's "you" (CR 603.3a). Scoped to a PLAYER
    -- recipient, which is the printed sentence -- CR 615.13 itself says nothing
    -- about who the damage was addressed to.
    --
    -- Not implemented: a link to the prevention effect that fired it, which a
    -- card printing "prevented this way" would need (#687).
    DamageToPlayerPrevented PlayerRelation.PlayerRelation
  | -- | CR 119.9: "whenever [a player] gains life" (Ajani's Pridemate). That
    -- rule rewrites the sentence as "whenever a SOURCE causes [a player] to gain
    -- life", which is why this matches GameEvent.LifeGained -- recorded only
    -- where a source caused the gain -- rather than any upward movement.
    --
    -- One fire per recorded event, which is where CR 702.15e's separate lifelink
    -- events are honoured. The amount is not part of the CONDITION -- any gain
    -- above 0 matches -- but is bound under Pawl.Engine.Binding.eventAmount for
    -- Sanguine Bond's "that much", and the gaining player under
    -- Pawl.Engine.Binding.triggerPlayer for False Cure's "that player", who under
    -- AnyPlayer is not CR 109.5's "you".
    --
    -- Losing life is a different event entirely (PlayerLosesLife), so a card
    -- bearing this stays silent for a loss and for prevented damage (CR 615.6).
    PlayerGainsLife PlayerRelation.PlayerRelation
  | -- | "Whenever [a player] loses life" (Exquisite Blood), against
    -- GameEvent.LifeLost. PlayerGainsLife's mirror in shape.
    --
    -- No CR 119.9 for this direction: the rules print no such rewriting for
    -- loss, so what this matches is settled by what the engine RECORDS, and the
    -- recording sites are the citation -- CR 119.3's effect
    -- (Pawl.Engine.Resolve's LoseLife), CR 119.2 / 120.3a's damage without
    -- infect (Pawl.Engine.Damage), and CR 119.4's paid life
    -- (Pawl.Engine.Event.payLife).
    --
    -- Three life-total facts that are NOT this event: CR 120.3b's infect damage
    -- gives poison counters instead; CR 615.6's prevented damage never leaves the
    -- total; CR 120.3c-e take a permanent's damage somewhere no life total is.
    -- The zero case needs no check here, every producer guarding its own zero.
    --
    -- The amount is bound under Pawl.Engine.Binding.eventAmount for Exquisite
    -- Blood's "that much", and the losing player under
    -- Pawl.Engine.Binding.triggerPlayer for Mindcrank's "that player".
    --
    -- Not implemented: CR 119.5's life-total SET, which would be a loss by that
    -- rule's own words whenever the new total is lower, records nothing.
    PlayerLosesLife PlayerRelation.PlayerRelation
  | -- | CR 714.2b generalized over the counter kind: "when one or more [kind]
    -- counters are put onto this permanent, if the number ... was less than N
    -- and became at least N". A THRESHOLD CROSSING, against a
    -- GameEvent.CountersPut whose before/after pair straddles N. Bearer-scoped.
    --
    -- The WHOLE sentence, intervening "if" included, rather than half of it on
    -- TriggeredAbility.intervening: both conjuncts describe the placement event
    -- rather than the board, so CR 603.4's second check could not find either
    -- changed. The consequence is that a chapter ability on the stack still
    -- resolves after its Saga's lore counters are removed, which is what CR
    -- 704.5s's exemption presupposes.
    --
    -- The Natural is N, the chapter number. CR 714.2c's "{rN1}, {rN2}" is two
    -- abilities sharing one effect, and the rule says the shorthand MEANS that,
    -- so a card writes two entries and nothing here represents it.
    --
    -- Not restricted to Sagas or to lore counters: the shape is the counter
    -- kind's, and Pawl.Engine.Saga is where the subtype is read.
    SelfCountersReached SelfCountersReached.SelfCountersReached
  | -- | CR 310.12b generalized over the counter kind: "when the LAST [kind]
    -- counter is removed from this permanent". SelfCountersReached's mirror,
    -- against a GameEvent.CountersRemoved going from one or more to none, and
    -- bearer-scoped alike. The whole sentence lives here for that constructor's
    -- reason: a permanent whose counters were removed and replaced before the
    -- ability resolved still had its last counter removed.
    --
    -- Takes no threshold, rule 310.12b stating none. Not restricted to battles
    -- or to defense counters; Pawl.Engine.Battle reads rule 310.
    SelfLastCounterRemoved (CounterKind.CounterKind Keyword.Keyword)
  | -- | "Whenever one or more [kind] counters are removed from this permanent"
    -- (Chandra, Fire Artisan). SelfLastCounterRemoved's any-amount mirror: same
    -- CounterKind payload, same bearer scope, and matched against the same
    -- GameEvent.CountersRemoved -- but with no reading of the AFTER count, so a
    -- removal that leaves counters behind matches and a removal that empties the
    -- object matches too.
    --
    -- The two do not collapse into each other in either direction. Rule 310.12b
    -- needs the last-counter reading, and Chandra needs the any-amount one; a
    -- board where three of four loyalty counters come off separates them.
    --
    -- No "one or more" conjunct, for SelfLastCounterRemoved's reason: the record
    -- exists only where something actually came off, an invariant stated on
    -- GameEvent.CountersRemoved itself.
    --
    -- The amount removed is bound under Pawl.Engine.Binding.eventAmount, which
    -- neither sibling stamps -- CR 603.2's "that much", read off the event's
    -- before/after pair rather than off the board, so CR 510.2's simultaneity
    -- carries into it: one batch of combat damage removing three counters is one
    -- trigger for three, not three for one.
    SelfCountersRemoved (CounterKind.CounterKind Keyword.Keyword)
  | -- | CR 601.2i: "whenever you cast a [type] spell" (Young Pyromancer). That
    -- rule's second sentence is the trigger event in as many words. Matched
    -- against GameEvent.SpellCast.
    --
    -- A FILTER over the spell rather than a PlayerRelation over the caster: the
    -- printed sentence narrows who cast it AND what it was, and only one of those
    -- is a player. "You cast" is Filter.ControlledBy You read against CR 109.5's
    -- "you" (CR 603.3a). Not self-scoped; "this spell" is SelfCast, for the zone
    -- reason that constructor gives.
    --
    -- The spell is read AS IT IS ON THE STACK, which CR 601.2i requires: that
    -- rule applies the effects modifying the spell's characteristics before it
    -- becomes cast, so the card in the hand is the wrong object.
    --
    -- The spell is bound under Pawl.Engine.Binding.castSpell and the caster
    -- under Pawl.Engine.Binding.triggerPlayer -- CR 112.2's controller, which
    -- Kambal, Consul of Allocation names without going through the spell, since
    -- CR 608.2h can have taken it away by resolution.
    --
    -- The TurnScope is a second axis beside the Filter, earned by Brineborn
    -- Cutthroat's "during an opponent's turn": whose turn it is is no
    -- characteristic of the spell, so Event.matchesTrigger reads it off the
    -- GameState. The ORDINAL is a fourth, earned by Clarion Spirit's "your
    -- second spell each turn" -- PlayerDrawsNthCard's question, kept a field
    -- because it narrows the same event.
    SpellCast SpellCast.SpellCast
  | -- | CR 601.2i read off the spell BEING cast -- "when you cast this spell"
    -- (Desolation Twin). Self-scoped.
    --
    -- The ZONE is what this constructor exists for: CR 113.6k puts a condition
    -- that cannot trigger from the battlefield in the zones it can, and CR
    -- 601.2a leaves the object on the stack.
    -- Pawl.Engine.Event.zonesTriggeredFrom answers that with a total case over
    -- this type; asking the same of a Filter would be a partial analysis of an
    -- open language, silently answering "battlefield" for shapes it had not
    -- anticipated.
    SelfCast
  | -- | CR 601.2c: "whenever this permanent becomes the target of a spell or
    -- ability [a player] controls" -- CR 702.21a's ward. Against
    -- GameEvent.BecameTarget; self-scoped, plus a relation reading the targeting
    -- object's controller.
    --
    -- Fires once per ANNOUNCEMENT of the bearer as a target, which is rule
    -- 601.2c's arity: one object may be chosen once per instance of the word
    -- "target", and each choice is a becoming.
    --
    -- Not implemented: CR 115.7's re-targeting effects, which would make a new
    -- object become a target (#1525).
    SelfBecomesTargeted PlayerRelation.PlayerRelation
  | -- | CR 601.2c from the PLAYER's side: "whenever you become the target of a
    -- spell or ability" (Dormant Gomazoa, Amulet of Safekeeping). Against
    -- GameEvent.BecameTarget whose `targeted` is a Recipient.ToPlayer equal to
    -- CR 109.5's "you" (CR 603.3a). The payload carries the narrowings the
    -- printings differ in; its own haddock says why each is read off the EVENT.
    ControllerBecomesTarget ControllerBecomesTarget.ControllerBecomesTarget
  | -- | CR 709.5h: "when you unlock this door" -- fires when the bearer is given
    -- the unlocked designation for the NAMED half, however it was given (CR
    -- 709.5d's entry, CR 709.5e's special action, CR 709.5f's keyword action).
    -- Self-scoped plus the half, which is what makes this the only condition
    -- that names one: a Room whose two doors both carry an unlock trigger has
    -- two abilities seeing the same event, and only the name separates them.
    --
    -- Reaching the half from the ABILITY instead would need the face an ability
    -- was printed on to survive Pawl.Engine.Projection's flattened list, and CR
    -- 709.4c combines the halves' abilities into one text box. Naming the door
    -- is the card stating a fact about itself, not the engine casing on which
    -- card it is.
    --
    -- CR 709.5i's "fully unlocks" is RoomFullyUnlocked.
    SelfHalfUnlocked CardName.CardName
  | -- | CR 709.5i: the permanent has one unlocked designation and gets the
    -- other, or has neither and gains both. Not self-scoped, which is the whole
    -- difference from SelfHalfUnlocked: Balemurk Leech watches every Room. Names
    -- no half, the rule being about the permanent.
    --
    -- The PlayerRelation reads the permanent's CONTROLLER, which is a reading of
    -- the printed "YOU fully unlock" and not the same sentence.
    --
    -- Not implemented: the actor. GameEvent.HalfUnlocked carries none, and CR
    -- 709.5f's keyword action reaches Pawl.Engine.Event.unlockHalf with no payer
    -- (#961).
    RoomFullyUnlocked PlayerRelation.PlayerRelation
  | -- | CR 603.1b: several conditions, ANY of which fires the ONE ability that
    -- bears them (Balemurk Leech). One ability rather than two is observable --
    -- CR 603.8's suppression, CR 603.3b's ordering and CR 603.1b's own "all of
    -- those conditions" clause all count abilities.
    --
    -- Read as "any". CR 603.1b's "all of those conditions have happened during a
    -- particular period" is a second thing a multi-condition ability can do, and
    -- nothing here does it.
    --
    -- Pawl.CardSpec's lint forbids a StateIs or a nested AnyOf inside one: state
    -- and event triggers are gathered by different scans
    -- (Pawl.Engine.Event.stateTriggers against matchesTrigger), and a flat list
    -- says everything a nested one could.
    AnyOf [TriggerCondition]
  | -- | CR 708.7 through CR 603.2: "when this creature is turned face up" (Skirk
    -- Marauder). Self-scoped.
    --
    -- No payload: CR 702.37e's morph cost, the player who took the special
    -- action and the characteristics regained are all things a printed ability
    -- could say "that much" about, and none does.
    SelfTurnedFaceUp
  | -- | CR 701.27e: "when this creature transforms into Blightsower Thallid",
    -- against GameEvent.Transformed. Self-scoped PLUS the name, which is
    -- TriggerCondition.SelfHalfUnlocked's shape and its reason -- every printing
    -- of this names a face, and naming it is the card stating a fact about
    -- itself rather than the engine casing on which card it is.
    --
    -- The name is not decoration. CR 701.27e admits an ability that triggers
    -- when an object "transforms into" an object with a SPECIFIED
    -- CHARACTERISTIC, and both faces of a card can carry one: Brutal Cathar
    -- fires on the turn back to the front face while Wildsong Howler fires on
    -- the turn away from it, and only the name tells the two events apart.
    -- Matched against the names the event carries (CR 201.1 / 707.2's projected
    -- ones, not the printed face's), which is CR 701.27e's "has the specified
    -- characteristic immediately after it does so".
    --
    -- CR 701.28a makes a convert follow CR 701.27a-f, so this condition fires on
    -- one too once there is such an opcode; see #698.
    --
    -- Not implemented: the BYSTANDER form CR 701.27e also admits, a card
    -- watching another permanent transform (Corruption of Towashi, Neglected
    -- Heirloom). The event carries everything such a condition would read, so
    -- what is missing is the arm rather than the record (#2050).
    SelfTransformedInto CardName.CardName
  | -- | CR 708.7's other written form read by a BYSTANDER (Aven Farseer).
    -- Filtered rather than self-scoped, so it reads the permanent's
    -- characteristics for a narrowed printing (Deathmist Raptor, Hamza);
    -- "another" would be `Not IsSource` inside the Filter. Aven Farseer's bare
    -- "a permanent" is Filter's trivial `And []`.
    --
    -- A LIVE read, unlike PermanentDies': CR 708.8 leaves the permanent on the
    -- battlefield with its copiable values back, so CR 603.10a's look-back
    -- exceptions do not reach this. Reading it BEFORE the turning would answer
    -- every narrowed form wrong, a face-down permanent having only the
    -- characteristics CR 708.2 lists.
    PermanentTurnedFaceUp (Filter.Filter Keyword.Keyword)
  | -- | A permanent the Filter admits GAINED THIS DESIGNATION -- CR 702.112b's
    -- renown (Valeron Wardens) and CR 701.37b's monstrous (Arbor Colossus),
    -- against GameEvent.BecameDesignated. The designation is a payload beside
    -- the Filter and is what keeps the readings apart.
    --
    -- PermanentTurnedFaceUp's shape and posture, including the LIVE read:
    -- nothing here is a zone change. The self forms (Relic Seeker) are this
    -- condition with Filter.IsSource, so no self-scoped constructor is needed.
    PermanentBecomesDesignated PermanentBecomesDesignated.PermanentBecomesDesignated
  | -- | CR 702.100b: the BEARER evolved (Renegade Krasis), against
    -- GameEvent.Evolved by an id comparison. Self-scoped and not filtered
    -- because both printings that read rule 702.100b's marker say "this
    -- creature".
    SelfEvolves
  | -- | CR 702.134c: the creature the bearer is ATTACHED TO mentored another
    -- (Aegis of the Legion), against GameEvent.Mentored, whose second id is
    -- bound under Pawl.Engine.Binding.mentoredCreature.
    --
    -- Attachment-scoped rather than self-scoped or filtered: the one printing is
    -- an Equipment, so its source is never the mentoring creature, and "equipped
    -- creature" is not a class a Filter could name (CR 301.5f). The same
    -- sentence Affected.Attached states for a static ability.
    --
    -- Vacuously False while the source is attached to nothing, or to a player
    -- (CR 303.4).
    AttachedCreatureMentors
  | -- | CR 702.149c: the BEARER trained (Savior of Ollenbock), against
    -- GameEvent.Trained by an id comparison. Self-scoped, rule 702.149c stating
    -- that form in as many words. The event is recorded only when a resolving
    -- training ability actually put a counter on, which is the rule's whole
    -- condition.
    SelfTrains
  | -- | CR 603.10a: "whenever a player sacrifices a permanent" (Mayhem Devil).
    -- One of the four look-back families that rule names.
    --
    -- NOT SelfDies or PermanentDies, even though CR 700.4 makes every sacrifice
    -- a death: this fires on the sacrifice AS a sacrifice (CR 701.21a's game
    -- action), which the zone change alone cannot say.
    -- GameEvent.PermanentSacrificed is recorded beside the Moved event rather
    -- than instead of it, so a death trigger still sees a sacrifice and this one
    -- does not see a destruction.
    --
    -- No payload: Mayhem Devil says "a PLAYER" and "a permanent", so a relation
    -- would carry PlayerRelation.AnyPlayer on every printing and a Filter would
    -- carry nothing. Not self-scoped.
    PermanentSacrificed
  | -- | CR 603.3b's second class: a trigger condition that IS another ability
    -- triggering. "Whenever the final chapter ability of a Saga you control
    -- triggers" (Historian's Boon), against GameEvent.AbilityTriggered, which
    -- Pawl.Engine.Engine appends for each gathered trigger before the batch goes
    -- on the stack.
    --
    -- Being in this class is why CR 603.3b has two passes;
    -- Pawl.Engine.Event.reactsToAbilityTriggering classifies exhaustively, so a
    -- new condition must decide which pass it takes.
    --
    -- Three things are checked together and none is separable into a Filter over
    -- the source: the ability must be a CHAPTER ability (CR 714.2b), its chapter
    -- must be its source's FINAL chapter number (CR 714.2d), and the source must
    -- be a Saga with chapter abilities (CR 714.1, CR 704.5s). The middle
    -- compares the event with the source's projection, which no Filter atom can
    -- express.
    --
    -- The PlayerRelation reads the TRIGGERED ability's controller against the
    -- watching ability's. No payload for which Saga or which chapter:
    -- Historian's Boon's effect names neither.
    SagaFinalChapterTriggers PlayerRelation.PlayerRelation
  | -- | CR 725.1: "whenever [a player] becomes the monarch" (Custodi Lich),
    -- against GameEvent.BecameMonarch. One constructor with a relation rather
    -- than two: "you" and "an opponent" are the same event read from two seats.
    --
    -- No Filter, which is CR 725.3 -- only one player can be the monarch, so a
    -- crowning names exactly one player.
    --
    -- Matched against the EVENT, never against how the crown was won: an entry
    -- trigger's crown, a targeted crown, CR 725.2's stolen crown and CR 725.4's
    -- departure reassignment all record the same event.
    PlayerBecomesMonarch PlayerRelation.PlayerRelation
  | -- | CR 603.7: "when you lose control of the creature" (Ray of Command).
    -- Fires when the permanent BOUND IN THE NAMED SLOT stops being controlled by
    -- "you", against GameEvent.ControlChanged.
    --
    -- The only condition that names a SLOT, because it is the only one whose
    -- subject is a particular object chosen earlier. A delayed ability's source
    -- is the SPELL that armed it (CR 603.7d), so a Self- condition would ask
    -- about Ray of Command on the stack; and a Filter would ask about "a
    -- creature" rather than THE creature. CR 603.7c's captured environment is
    -- what remembers which.
    --
    -- "You" is CR 603.7d's controller of the spell as it resolved, matched
    -- against the player the event says control LEFT. Where it went is not read.
    --
    -- An empty slot never matches, which is CR 608.2b's fizzle rather than a
    -- guard: a spell whose only target became illegal arms nothing.
    --
    -- A permanent LEAVING the battlefield is not a match,
    -- GameEvent.ControlChanged being sampled on the battlefield only, so the
    -- entry stays armed rather than spending CR 603.7b's one shot --
    -- unobservable, the ability could only act on an object CR 400.7 has
    -- replaced.
    LoseControlOfBound SlotName.SlotName
  | -- | CR 309.4c: "When you move your venture marker into this room", against
    -- GameEvent.VentureMarkerEntered naming the bearer and this room.
    -- Self-scoped through the bearer AND the room index, SelfHalfUnlocked's
    -- shape and reason: a dungeon has one ability per room, all borne by the
    -- same card.
    --
    -- Never written by card data: Pawl.Engine.Dungeon mints one per room of
    -- Pawl.Types.Face.rooms, which is why Pawl.Types.DungeonRoom carries a bare
    -- Modal rather than a whole TriggeredAbility.
    RoomEntered RoomIndex.RoomIndex
  | -- | CR 701.22d: "whenever you scry" (Matoya, Archon Elder), against
    -- GameEvent.Scried. Counts SCRIES rather than cards, which is why it reads
    -- its own event: CR 701.22a's reorder records nothing, and a scry that moved
    -- nothing still fires this. CR 701.22b's scry 0 records no event.
    PlayerScries PlayerRelation.PlayerRelation
  | -- | CR 701.25d, PlayerScries' twin, against GameEvent.Surveiled. A surveil
    -- that put nothing into a graveyard fires it just the same, which is what
    -- keeps it apart from a condition built on CR 701.25a's zone changes. CR
    -- 701.25c's surveil 0 fires nothing.
    PlayerSurveils PlayerRelation.PlayerRelation
  | -- | CR 702.170a / 702.170c: "when this card becomes plotted" (Aloe
    -- Alchemist), against GameEvent.Plotted naming the bearer. Self-scoped and
    -- nullary. Watched for from EXILE, which is where both routes leave the
    -- card -- CR 702.170b's special action exiles it, and rule 702.170c's
    -- effect acts on a card already there.
    SelfBecomesPlotted
  | -- | CR 701.44b: "whenever a creature you control explores" (Wildgrowth
    -- Walker), against GameEvent.Explored, with the Filter applied to the
    -- EXPLORER and through last known information, PermanentDies' posture. Fires
    -- once per completed explore, including one whose library was empty, which
    -- keeps it apart from a condition built on CR 701.44a's steps.
    PermanentExplores (Filter.Filter Keyword.Keyword)
  | -- | CR 701.43d \/ 607.2h: "when you do" beside "you may exert this creature
    -- as it attacks" (Glory-Bound Initiate), against GameEvent.Exerted by an id
    -- comparison. Self-scoped and nullary, CR 701.43d stating the linked form.
    --
    -- CR 607.2h's linkage holds by construction rather than by a link field: the
    -- event is recorded only by the CR 508.1g payment on THIS permanent.
    --
    -- Not implemented: a card bearing two exert paragraphs, whose two triggers
    -- would each see both exerts.
    SelfExerted
  | -- | CR 701.3a read by the HOST: "whenever an Aura becomes attached to this
    -- creature" (Bramble Elemental), against GameEvent.BecameAttached, whose
    -- `host` is compared with the bearer while the Filter reads the ATTACHMENT.
    -- A self-scoped bearer PLUS a Filter, the two objects playing different
    -- parts.
    --
    -- A LIVE read of the attachment, PermanentTurnedFaceUp's posture: attaching
    -- is no zone change, and the one route that is -- an Aura arriving already
    -- attached -- leaves the permanent on the battlefield.
    --
    -- CR 702.26j is satisfied by where the event is emitted:
    -- Pawl.Engine.Phasing writes Object.attachedTo without going through the
    -- funnel, so phasing records nothing to match.
    --
    -- Not implemented: the other scope, the bearer as the ATTACHMENT (Enormous
    -- Energy Blade), which needs its own constructor and a binding for "that
    -- creature" (gap #1837).
    SelfBecomesAttachedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 603.12's reflexive triggered ability: "you may sacrifice a Clue. WHEN
    -- YOU DO, target instant or sorcery card in your graveyard gains flashback"
    -- (The Fugitive Doctor). Nullary, and it matches no GameEvent at all.
    --
    -- CR 603.12 routes a reflexive through rule 603.7, so its carrier is a
    -- delayed entry -- but with the exception that it is "checked immediately
    -- after being created" and triggers "based on whether the trigger event or
    -- events occurred earlier during the resolution of the spell or ability that
    -- created them". Both halves are discharged STRUCTURALLY rather than by
    -- re-reading the log: the only thing that appends the entry is the CR 118.12
    -- pay gate's IfPaid branch, which runs exactly when the cost was paid, so by
    -- the time an entry with this condition exists its trigger event has already
    -- occurred. Pawl.Engine.Event.delayedPending therefore fires it once, on no
    -- event, at the next gather -- which is CR 603.3's "the next time a player
    -- would receive priority", so its targets are chosen (CR 603.3d) after the
    -- payment rather than as the creating ability went on the stack.
    --
    -- CR 603.12a's "paying that cost one or more times causes the reflexive
    -- triggered ability to trigger only once" holds by construction: one clause
    -- arms one entry, and an entry with no stated duration is spent by its one
    -- firing (CR 603.7b).
    --
    -- Not implemented: CR 603.12a's FIRST sentence, "once for each of those
    -- times", which rule 603.12's other printed form ("when [something happens]
    -- this way") reaches -- that event is no payment and can occur several times
    -- in one resolution, where this fires once (#2121).
    Reflexive
  deriving (Eq, Ord, Show)
