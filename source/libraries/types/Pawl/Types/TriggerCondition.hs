module Pawl.Types.TriggerCondition where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.CounterKind as CounterKind
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

-- | CR 603.2: the pattern that fires a triggered ability. Only Pawl.Engine.Event may case
-- on it for RULES purposes; Pawl.Codec also cases on every constructor, but only
-- as the JSON data boundary, not to decide game behaviour.
data TriggerCondition
  = -- | CR 603.6a: "when this ... enters" -- fires when the object BEARING the
    -- ability enters. Self-scoped: the scan checks every permanent, so the
    -- bearer's identity is part of the match. PermanentEnters below is the other
    -- half of that same sentence, kept SEPARATE rather than written as
    -- `PermanentEnters IsSource`: this arm is a bare comparison of ids, while that
    -- one has to READ the entrant's characteristics, and reading them can come up
    -- empty for an entrant that ceased without a zone change ever filing last
    -- known information (see Pawl.Engine.Event.matchesTrigger).
    SelfEnters
  | -- | CR 603.6a's SECOND written form -- "whenever a [type] enters" -- fires when
    -- ANY permanent the Filter admits enters, whoever bears the ability. The
    -- bearer is the Filter.Context's source and its controller the perspective.
    --
    -- Named for the rule, NOT "OtherEnters": the bearer is not excluded here,
    -- since CR 603.6a checks the newcomers too. Soul Warden's "whenever ANOTHER
    -- creature enters" writes that exclusion into the Filter as `Not IsSource`
    -- (#163) rather than into a parallel exclusion flag.
    PermanentEnters (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2b: "at the beginning of [each|your] <step>". Matched against a
    -- GameEvent.StepBegan; the TurnScope decides whose turn qualifies.
    --
    -- The ACTIVE player comes across in the reserved
    -- Pawl.Engine.Binding.triggerPlayer slot, stamped by
    -- Pawl.Engine.Event.eventBindings: "each player's upkeep" goes on to say "that
    -- player", and CR 109.5's "you" is the controller instead.
    StepBegins StepBegins.StepBegins
  | -- | CR 603.8: a STATE trigger -- it fires whenever its condition is true, not
    -- when an event occurs, and not again until the ability has left the stack.
    -- That is why Pawl.Engine.Event derives armedness from the stack rather than
    -- storing it.
    StateIs Condition.Condition
  | -- | CR 603.2 / 509-510: the bearer dealt combat damage to a player, read off
    -- the DamageDealt event history.
    SelfDealsCombatDamageToPlayer
  | -- | CR 120.3: the bearer WAS DEALT damage -- the enrage trigger's event
    -- (Ripjaw Raptor's "whenever this creature is dealt damage"). The mirror of
    -- SelfDealsCombatDamageToPlayer above, off the same DamageDealt history, with
    -- the identity check moved from the event's source to its recipient.
    --
    -- NO DamageKind and no source Filter, unlike every neighbouring damage
    -- condition: the printed phrase qualifies the damage in no way, so combat
    -- damage and a Prodigal Sorcerer's ping alike fire it.
    --
    -- Enrage is an ability word (CR 207.2c) with no rules meaning of its own, so
    -- nothing about it reaches Pawl.Types.Keyword; this is an ordinary triggered
    -- ability that happens to be printed after an italic word.
    --
    -- ONE fire per recorded event, not one per batch: CR 510.2 deals a combat
    -- damage step's damage simultaneously and Pawl.Engine.Damage records a
    -- DamageDealt per surviving event, so a creature blocked by two others
    -- triggers this twice. Pinned by Pawl.TriggerSpec.
    --
    -- Binds how much that one event dealt under Pawl.Engine.Binding.eventAmount,
    -- which is Coalhauler Swine's "that much". The DAMAGER gets no slot -- no
    -- printing reads it here -- and neither does the recipient, who is the bearer.
    SelfIsDealtDamage
  | -- | CR 603.2 / 509-510 again, read by a BYSTANDER: a permanent the Filter
    -- admits dealt combat damage to a player -- Tovolar, Dire Overlord's
    -- "whenever a Wolf or Werewolf you control deals combat damage to a player".
    -- The Filter carries the "you control" conjunct, as every other one does.
    --
    -- Beside SelfDealsCombatDamageToPlayer rather than replacing it. The self form
    -- is not a card's: rules 702.70a and 702.112a mint it (Pawl.Engine.Keyword's
    -- poisonous and renown), and an id comparison states those without reading the
    -- damager's characteristics at all -- where this one must, so it goes through
    -- CR 608.2h's last known information for a trampler that died to its blocker
    -- in the same event. SelfTurnedFaceUp beside PermanentTurnedFaceUp is the
    -- standing pair for that; Filter.IsSource would collapse the two here as it
    -- does for PermanentBecomesDesignated, and buys a minted keyword nothing.
    --
    -- Binds CR 510.2's damager under Pawl.Engine.Binding.combatDamager, which the
    -- self form needs no slot for -- there the damager is the bearer -- and the
    -- amount it dealt under Pawl.Engine.Binding.eventAmount, which is Shroofus
    -- Sproutsire's "that many". The damaged player gets none: the event names one,
    -- and nothing in the pool reads it here (#1175).
    PermanentDealsCombatDamageToPlayer (Filter.Filter Keyword.Keyword)
  | -- | CR 725.2: a creature dealt combat damage to the monarch. NOT bearer-scoped
    -- (any creature); matched only via Pawl.Engine.Monarch.inherentMatch, never through a
    -- card's bearer.
    CreatureDealtCombatDamageToMonarch
  | -- | CR 702.179d: "whenever one or more opponents lose life during your turn".
    -- The inherent speed-increase ability's event, and CreatureDealtCombatDamageToMonarch's
    -- sibling in every respect -- borne by no card, matched only via
    -- Pawl.Engine.Speed.inherentPending, never through a card's bearer, which is
    -- why Pawl.Engine.Event.matchesTrigger answers False for it.
    --
    -- Neither of the rule's other two clauses is here. "If your speed is less than
    -- 4" is CR 603.4's intervening "if", which rides
    -- Pawl.Types.TriggeredAbility.intervening; "this ability triggers only once
    -- each turn" is a limit on the ABILITY rather than a description of the event,
    -- which Pawl.Types.TriggeredAbility.limit now states and
    -- GameState.speedIncreasedThisTurn still enforces here. Folding either in
    -- would make this constructor mean one ability instead of one event.
    OpponentLostLifeDuringYourTurn
  | -- | CR 702.29c: "when you cycle this card". Self-scoped like SelfEnters. The
    -- bearer is the card in the zone it landed in, which is that rule's second
    -- sentence -- the graveyard, for every printing today.
    SelfCycled
  | -- | CR 702.94a: "when you reveal this card this way" -- the triggered half of
    -- miracle, linked (CR 603.11) to the static half that opened the window.
    -- Self-scoped like SelfCycled: the bearer is the card that was revealed, and
    -- CR 701.20b left it exactly where it was, so it is still in its owner's
    -- HAND. That is the whole of why this condition needs a hand candidate source
    -- (CR 113.6k) where SelfCycled needed a graveyard one.
    --
    -- "THIS WAY" is the link, and it is why the match is not merely "this card
    -- was revealed": a miracle card shown by any other effect must not fire. The
    -- event carries Pawl.Types.RevealCause for that, so this condition reads the
    -- cause rather than trusting that a reveal of a miracle card can only have
    -- been a miracle reveal.
    --
    -- No PlayerRelation. CR 702.94a's reveal is one a player makes from their OWN
    -- hand as they draw, and CR 113.8 makes the card's owner the ability's
    -- controller, so "you" and the revealer are the same seat by construction.
    SelfRevealedForMiracle
  | -- | CR 701.9a: "when you discard this card" -- Bartered Cow's second trigger
    -- condition. Self-scoped like SelfCycled: the bearer is the card that was
    -- discarded, which CR 701.9a has already moved to its owner's graveyard, so
    -- the event's CR 400.7 incarnation is the object the scan offers.
    --
    -- The CAUSE is deliberately not read, where SelfCycled reads it. CR 702.29a
    -- makes cycling a discard, so a card discarded to pay a cycling cost fires
    -- this too; CR 702.29d then bounds it to once, which the single
    -- GameEvent.Discarded funnel supplies by construction. No printing carries
    -- both this condition and cycling, so only Pawl.TriggerSpec's direct drive of
    -- Event.discard reaches the cycling half.
    --
    -- No PlayerRelation. CR 701.9a discards from the card's OWNER's hand, and CR
    -- 113.8 makes that owner the ability's controller, so "you" and the
    -- discarding player are the same seat by construction -- SelfRevealedForMiracle's
    -- argument one rule over.
    SelfDiscarded
  | -- | CR 701.9a: "whenever [a player] discards a card" -- Megrim's. Matched
    -- against GameEvent.Discarded, whose PlayerId is the discarding player; the
    -- PlayerRelation reads that player against CR 109.5's "you", the ability's
    -- controller (CR 603.3a). NOT self-scoped, unlike SelfCycled and
    -- SelfDiscarded above: the bearer is a bystander watching someone else's
    -- hand.
    --
    -- The DiscardCause is deliberately not part of this condition. CR 702.29a
    -- makes cycling a discard, so a cycled card must fire this; CR 702.29d bounds
    -- it to once, which the single Discarded event supplies by construction.
    PlayerDiscards PlayerRelation.PlayerRelation
  | -- | CR 121.1: "whenever [a player] draws their Nth card each turn" -- Erudite
    -- Wizard's whole text. Matched against GameEvent.Drew, whose PlayerId is the
    -- drawing player and whose Natural is which of that player's draws this turn
    -- it was; the PlayerRelation reads the drawer against CR 109.5's "you", the
    -- ability's controller (CR 603.3a). PlayerDiscards' shape, not a Self-
    -- condition's: the bearer is a creature watching its own controller's draws
    -- and has nothing to do with the card drawn.
    --
    -- "Each turn" is not a field. CR 121.2 makes every draw its own event and the
    -- ordinal the event carries is already per-turn -- GameState.drawsThisTurn is
    -- cleared at the handoff -- so the count restarts without this condition
    -- saying so. What the Natural picks out is WHICH draw, which is the same
    -- question CR 702.94a asks with the answer 1 -- though miracle is not this
    -- condition: it triggers on the REVEAL that CR 121.9's window offers, which
    -- is SelfRevealedForMiracle above.
    --
    -- EQUALITY, not "at least": Erudite Wizard fires on the second draw and on no
    -- other, so a turn with four draws fires it once. A card reading "your second
    -- and each subsequent card" would be a different condition, and none is
    -- printed.
    PlayerDrawsNthCard PlayerDrawsNthCard.PlayerDrawsNthCard
  | -- | CR 508.3a: "whenever [a creature] attacks" -- Hanweir Garrison's.
    -- Self-scoped like SelfEnters.
    --
    -- DECLARED is the whole content of the condition, not a synonym for
    -- "attacking": CR 508.3a exempts a creature put onto the battlefield
    -- attacking, and CR 508.4 says such a creature never attacked. So this matches
    -- GameEvent.AttackerDeclared, which only the declaration appends, and never
    -- the combat record. No attack TARGET is compared against by THIS arm; CR
    -- 508.3a's second sentence is CreatureAttacksYou below, which compares the
    -- DEFENDING PLAYER instead, and SelfAttacksPlayerWithMostLife reads the target
    -- for rule 702.105a's own comparison rather than as a general form. What the
    -- event does carry alongside the attacker is CR 508.5's defending player, whom
    -- Pawl.Engine.Event.eventBindings binds under
    -- Pawl.Engine.Binding.triggerPlayer for CR 702.86a's annihilator to read --
    -- a BINDING rather than part of the match, since this condition fires on
    -- every declaration whoever is defending.
    --
    -- The TriggerFrequency is Aurelia, the Warleader's "for the first time each
    -- turn" -- a payload rather than a sibling condition, because it narrows this
    -- same trigger event rather than naming a different one.
    SelfAttacks TriggerFrequency.TriggerFrequency
  | -- | CR 508.3a again, with a companion required: "whenever this creature and at
    -- least one OTHER creature [the Filter admits] attack" -- rule 702.149a's
    -- training. Self-scoped like SelfAttacks, matched against the same
    -- GameEvent.AttackerDeclared, and DECLARED for that condition's reason.
    --
    -- A SIBLING of SelfAttacks rather than a Filter payload on it, because the two
    -- ask about different objects: SelfAttacks' subject is the bearer, and this
    -- one's Filter is a predicate over everybody ELSE the same declaration named.
    -- CreatureAttacksAlone is the third shape -- a bystander's reading of a
    -- declaration the bearer need not be in -- and this one sits between them:
    -- the bearer must attack, and somebody else must too.
    --
    -- AT LEAST ONE, so the Filter is asked existentially; rule 702.149a triggers
    -- once however many companions qualify, since CR 603.2 counts occurrences of
    -- the EVENT and one declaration is one event.
    --
    -- The companions come from the combat record and not from the event log,
    -- which is what makes a second combat phase right: the log keeps a turn's
    -- declarations, where Pawl.Types.Combat is cleared per combat phase.
    -- Reading the record is exact at this moment despite CR 508.4 -- a creature
    -- put onto the battlefield attacking needs an effect to resolve, and CR 508.2b
    -- puts these triggers on the stack before any player gets priority.
    --
    -- Nothing is bound: rule 702.149a's payload names only "this creature", so no
    -- companion has to be pointed at afterwards. Which one qualified is therefore
    -- not observable, and there is nothing to ask the controller.
    SelfAttacksWithAnother (Filter.Filter Keyword.Keyword)
  | -- | CR 506.5: "whenever a creature you control attacks alone" -- rule
    -- 702.83a's exalted. SelfAttacks read by a BYSTANDER and narrowed to a
    -- one-creature declaration, matched against the same
    -- GameEvent.AttackerDeclared.
    --
    -- NOT self-scoped, and that is the constructor's whole point: rule 702.83a
    -- says "a creature YOU CONTROL", so an Aven Squire held back while another
    -- creature attacks alone still triggers, and the attacker is not the bearer.
    -- PermanentEnters' shape rather than SelfAttacks': the bearer frames the
    -- match -- it is the Filter context's source, its controller CR 109.5's "you"
    -- (CR 603.3a) -- rather than being it.
    --
    -- ALONE rides the constructor rather than the Filter, because CR 506.5 makes
    -- it a fact about the DECLARATION and not a characteristic of the creature --
    -- the same argument CombatRestriction.CantAttackAlone makes one step earlier.
    -- Pawl.Types.Filter's atoms are all characteristics of a candidate, so no
    -- Filter can say it. The number is read off the count
    -- GameEvent.AttackerDeclared carries, which is why rule 702.83b's "in a given
    -- combat phase" holds across an extra combat phase.
    --
    -- No TriggerFrequency: rule 702.83a states none, and CR 506.1 gives a combat
    -- phase one declare attackers step, so the event cannot repeat within one.
    --
    -- The attacker is bound under Pawl.Engine.Binding.attackingCreature for rule
    -- 702.83a's "that creature" to read -- a different object from the bearer, as
    -- SelfBecomesBlockedBy's blocker is.
    CreatureAttacksAlone (Filter.Filter Keyword.Keyword)
  | -- | CR 508.3a's second sentence read by a BYSTANDER: Marchesa's Decree's
    -- "whenever a creature attacks you or a planeswalker you control". Matched
    -- against the same GameEvent.AttackerDeclared, and DECLARED for SelfAttacks'
    -- reason (CR 508.4).
    --
    -- Per declared attacker, so three attackers fire it three times. CR 508.3b's
    -- "[a player] is attacked" is once per DECLARATION instead -- a different
    -- arity, needing the grouped event pawl does not have (gap #538).
    --
    -- NULLARY where CreatureAttacksAlone carries a Filter: CR 508.1a lets only a
    -- creature be declared as an attacker, so "a creature" is no predicate to
    -- check. A card narrowing the attacker would add a Filter here rather than a
    -- sibling constructor.
    --
    -- "You or a planeswalker you control" is ONE test: CR 508.5/508.5a make the
    -- defending player the attacked player or the attacked planeswalker's
    -- controller, and that is exactly the field the event carries. So this reads
    -- the event and never the board, unlike SelfAttacksPlayerWithMostLife. CR
    -- 109.5/603.3a fix "you" as the ability's controller.
    --
    -- Not self-scoped, PlayerDiscards' posture: the bearer is an enchantment
    -- watching somebody else's attack. The attacker is bound under
    -- Pawl.Engine.Binding.attackingCreature for "that creature's controller",
    -- CreatureAttacksAlone's arrangement.
    CreatureAttacksYou
  | -- | CR 702.105a: "whenever this creature attacks the player with the most life
    -- or tied for most life" -- dethrone's. SelfAttacks narrowed by a fact about
    -- WHOM the bearer attacked, matched against the same
    -- GameEvent.AttackerDeclared and self-scoped the same way.
    --
    -- The attacked player comes from Combat.attackers rather than from the event,
    -- and that is the whole content of the narrowing: the event carries CR 508.5's
    -- DEFENDING player, who is also the controller of an attacked planeswalker or
    -- the protector of an attacked battle, where rule 702.105a says "attacks THE
    -- PLAYER". Only AttackTarget.OfPlayer satisfies it. Reading the record at this
    -- moment is exact for SelfAttacksWithAnother's reason: CR 508.2b puts these
    -- triggers on the stack before any player gets priority.
    --
    -- MOST LIFE is compared across every player still in the game (CR 800.4a), the
    -- bearer's controller included -- attacking an opponent on 20 does not trigger
    -- while you sit on 25. "Or tied for most life" is what makes the comparison
    -- non-strict, so the arm needs no payload to tell the two readings apart.
    --
    -- Read off the game state as the trigger is matched, not through
    -- Pawl.Types.Condition: an intervening "if" (CR 603.4) would be checked AGAIN
    -- on resolution (CR 608.2a), and rule 702.105a states no "if" -- a life gain in
    -- response must not stop the counter.
    --
    -- No payload and no Filter: rule 702.105a states one comparison and names no
    -- characteristic of the attacker.
    SelfAttacksPlayerWithMostLife
  | -- | CR 509.3a: "whenever [a creature] blocks" -- Pride Guardian's.
    -- SelfAttacks' mirror, self-scoped like SelfEnters.
    --
    -- DECLARED, for that condition's reason read on the blocking side: CR 509.4
    -- says a creature put onto the battlefield blocking never "blocked", so this
    -- matches GameEvent.BlockerDeclared and never Combat.blockers.
    --
    -- No TriggerFrequency: rule 509.3a's "only once each combat for that
    -- creature, even if it blocks multiple creatures" is the GROUPING of
    -- GameEvent.BlocksDeclared, which Pawl.Engine.Combat.declareBlockers records
    -- once per blocking creature however many attackers it took. A real dedup,
    -- since Foriysian Brigade blocks two.
    --
    -- The ATTACKER the event also carries is not compared here, and not bound:
    -- CR 509.3b's "blocks a creature" is the form that names it, and that is
    -- SelfBlocksCreature below. CR 509.3c's is SelfBecomesBlocked and CR 509.3d's
    -- is SelfBecomesBlockedBy, which read this same event from the attacker's
    -- side.
    SelfBlocks
  | -- | CR 509.3b: "whenever [a creature] blocks a creature" -- Loyal Sentry's.
    -- SelfBlocks with the attacker named, self-scoped and DECLARED for the same
    -- reasons, and matched against the same GameEvent.BlockerDeclared.
    --
    -- The whole difference from SelfBlocks is the EVENT and the BINDING: this
    -- reads the per-pair GameEvent.BlockerDeclared, which is rule 509.3b's "once
    -- for each attacking creature the creature with the ability blocks", and puts
    -- the attacker into Pawl.Engine.Binding.blockedCreature for the payload's
    -- "that creature" to read.
    --
    -- No Filter over the attacker, unlike SelfBecomesBlockedBy. The printings that
    -- narrow it -- Netcaster Spider's "a creature with flying" -- are not served
    -- (#1253); SelfBlocksOneOrMore's Filter is a different arity and does not
    -- cover them.
    --
    -- Rule 509.3b's other producer, an effect that causes the bearer to block,
    -- records no event and so does not reach this (#1146). CR 509.4's creature
    -- put onto the battlefield blocking is excluded by the rule itself, and by
    -- the same construction that excludes it from SelfBlocks.
    SelfBlocksCreature
  | -- | CR 509.3e: "whenever [a creature] blocks two or more creatures" --
    -- Lairwatch Giant's. SelfBlocks with a floor on the number, matched against
    -- the same grouped GameEvent.BlocksDeclared and self-scoped the same way.
    --
    -- AT LEAST, never exactly: rule 509.3e's last sentence says the form covers
    -- "at least a certain number", and every printing states it as "two or more".
    -- An exactly-N printing would be a second arm rather than a reading of this
    -- one.
    --
    -- The Natural is the floor. Two on every card in the pool; carried because
    -- the rule is written about a number rather than about two.
    SelfBlocksAtLeast Natural.Natural
  | -- | CR 509.3e: "whenever [a creature] blocks one or more [F] creatures" --
    -- Serra Inquisitors' first half, whose Filter is "black". SelfBlocksAtLeast
    -- with the number spent on a QUALITY instead: rule 509.3e's last sentence
    -- covers "at least a certain number", and one is the number every filtered
    -- printing states. Matched against the same grouped GameEvent.BlocksDeclared
    -- and self-scoped the same way.
    --
    -- The grouping is what the printed "one or more" asks for: the bearer's whole
    -- declaration fires this once, however many admitted creatures it blocked.
    -- CR 509.3b's per-attacker form is SelfBlocksCreature above, and two admitted
    -- attackers is the board that tells them apart.
    --
    -- No Natural, unlike SelfBlocksAtLeast: no printing filters AND counts past
    -- one, and a floor no card states would be a number no board could observe.
    -- The two arms stay separate for that reason rather than merging into a
    -- counted-and-filtered one.
    --
    -- The Filter is a predicate over each ATTACKER the bearer blocked, read from
    -- Pawl.Types.Combat's declaration record rather than from the event, which
    -- carries only how many. Read at the scan, which is rule 509.3f's "at the
    -- point blockers are declared" for SelfBecomesBlockedBy's reason.
    --
    -- Rule 509.3e's "effects that add or remove blockers" reach neither this nor
    -- SelfBlocksAtLeast: the declaration is the only producer (#1146).
    SelfBlocksOneOrMore (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3c: "whenever [a creature] becomes blocked" -- Sacred Prey's. The
    -- ATTACKING side of SelfBlocks, and self-scoped the same way.
    --
    -- Matched against GameEvent.AttackerBlocked, which CR 509.1's declaration
    -- produces one of per attacker that got at least one blocker. That grouping
    -- is rule 509.3c's "only once each combat for that creature, even if it's
    -- blocked by multiple creatures" -- the same grouping SelfBlocks reads on the
    -- blocking side.
    --
    -- Rule 509.3c's SECOND producer reaches it too: an effect that says the
    -- creature becomes blocked (Effect.BecomesBlocked, Curtain of Light) records
    -- the same event, and the rule's "only if the attacking creature was an
    -- unblocked creature at that time" is Pawl.Engine.Combat.becomeBlocked's own
    -- guard. The third -- a creature put onto the battlefield as a blocker (CR
    -- 509.4) -- has no producer in the pool (#1387).
    --
    -- No blocker is bound: rule 509.3c's form names none. CR 509.3d's does, and
    -- that is SelfBecomesBlockedBy below. CR 508.5's defending player IS bound,
    -- the event carrying it -- rule 702.130a's afflict is what reads it.
    SelfBecomesBlocked
  | -- | CR 509.3d: "whenever [a creature] becomes blocked by a creature" -- rule
    -- 702.25a's flanking, whose Filter is "without flanking". Self-scoped on the
    -- ATTACKING side like SelfBecomesBlocked, and matched against
    -- GameEvent.BlockerDeclared's PAIR rather than GameEvent.AttackerBlocked:
    -- rule 509.3d "triggers once for each creature that blocks the specified
    -- creature", which is one declaration event per blocker.
    --
    -- That arity is the whole difference from SelfBecomesBlocked above, which
    -- reads the grouped event and fires once however many blockers there are. Two
    -- blockers is the board that tells them apart.
    --
    -- The Filter is a predicate over the BLOCKER, read at the scan -- which is
    -- rule 509.3f's "at the point it becomes a blocking creature", since CR
    -- 509.2a puts the triggers on the stack before any player gets priority and
    -- nothing can change the blocker in between. The bearer frames the match
    -- rather than being it, as PermanentEnters' does: it is the Filter context's
    -- source and its controller the perspective CR 109.5 gives "you".
    --
    -- The blocker is bound under Pawl.Engine.Binding.blockingCreature for rule
    -- 702.25a's "the blocking creature" to read: it is a different object from the
    -- bearer, unlike SelfBecomesBlocked's, where the event names nobody but the
    -- attacker.
    --
    -- Rule 509.3d's other two producers -- an effect that adds a blocker, and a
    -- creature put onto the battlefield blocking, which this one form DOES
    -- trigger for -- have no producer in the pool and record no event (#1146).
    SelfBecomesBlockedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e: "whenever [a creature] becomes blocked by one or more [F]
    -- creatures" -- Serra Inquisitors' second half, and SelfBlocksOneOrMore read
    -- from the ATTACKING side. Self-scoped like SelfBecomesBlocked, and matched
    -- against the grouped GameEvent.AttackerBlocked for the same reason its
    -- blocking twin reads GameEvent.BlocksDeclared: the printed "one or more"
    -- fires once for the whole declaration.
    --
    -- That grouping is the whole difference from SelfBecomesBlockedBy above,
    -- which reads the per-blocker pair and fires once for each. Two admitted
    -- blockers is the board that tells them apart.
    --
    -- No blocker is bound. The form names a SET ("one or more"), not an object,
    -- and the pool's payloads that reach into that set are Equipment (Godsend's
    -- "one of those creatures"), whose attachment-scoped triggers pawl has no
    -- shape for; Serra Inquisitors' payload names only itself.
    --
    -- Rule 509.3e's other producers -- an effect that adds a blocker, and a
    -- creature put onto the battlefield blocking -- record no event (#1146), as
    -- for every condition in this family.
    SelfBecomesBlockedByOneOrMore (Filter.Filter Keyword.Keyword)
  | -- | "Whenever this creature attacks and isn't blocked" -- Eternal of Harsh
    -- Truths', and CR 702.68a's frenzy, which Pawl.Engine.Keyword.frenzy mints
    -- with this condition. The glossary entry for that phrase sends it to CR 509.1h: the
    -- ability triggers when the creature becomes an unblocked attacking
    -- creature, which is one branch of the same turn-based action
    -- SelfBecomesBlocked reads the other branch of. Self-scoped like it.
    --
    -- Matched against GameEvent.AttackerUnblocked, which
    -- Pawl.Engine.Combat.declareBlockers records once per attacker the
    -- declaration left with no blockers. Once per attacker is the only arity the
    -- phrase can have -- there is no per-blocker reading of "isn't blocked" --
    -- so this needs no once-per-combat dedup of SelfBecomesBlocked's kind; the
    -- event's own grouping is it.
    --
    -- The condition is NOT "the bearer attacked, and is not blocked right now".
    -- Rule 509.1h fixes the status at the declaration and its last sentence
    -- keeps it fixed, so a state test at any later point would answer for an
    -- attacker whose blockers all died. Reading an event recorded at the
    -- declaration is what makes that distinction hold.
    --
    -- Nothing is bound. The printed forms that name the defending player (Crypt
    -- Cobra) are not in the pool, and GameEvent.AttackerUnblocked carries no
    -- player to bind for them; the attacker is the bearer, already CR 113.7a's
    -- source.
    SelfAttacksUnblocked
  | -- | CR 603.6 (a zone-change trigger): "when this card is put into your
    -- graveyard from your library" -- Narcomoeba's. Self-scoped like SelfEnters.
    --
    -- The bearer is the card AS IT NOW IS IN THE GRAVEYARD -- CR 400.7's new
    -- incarnation, which CR 400.7e lets the ability find since a graveyard is
    -- public (CR 400.2) -- because CR 113.6k puts the ability wherever it can
    -- trigger from, and a card cannot be put into a graveyard from a library while
    -- it is on the battlefield.
    --
    -- No zone pair is carried, so this is not a general "moved from A to B": the
    -- two zones are exactly what makes CR 113.6k apply, and a second card wanting
    -- a different pair is the one that must generalise this.
    -- SelfPutIntoGraveyardFromAnywhere below is a card wanting NO origin zone,
    -- and is a sibling rather than that generalisation: Narcomoeba's printed
    -- sentence names a library and must stay silent for a discard.
    --
    -- The printed "your ... your" needs no controller check. A card is always put
    -- into its OWNER's graveyard from its OWNER's library, and a card in a
    -- graveyard has no controller (CR 108.4), so CR 113.8 makes the ability's
    -- controller that same owner.
    SelfPutIntoGraveyardFromLibrary
  | -- | CR 603.6: "when this card is put into a graveyard from anywhere" -- Serra
    -- Avatar's. Self-scoped like SelfEnters, and the WIDEST of the three
    -- put-into-a-graveyard conditions: any zone at all is a legal origin, so a
    -- discard, a mill, a countered spell and a death all fire it.
    --
    -- NOT a leaves-the-battlefield ability, which is CR 603.6c's own last
    -- sentence saying so in as many words, and the reason this is a sibling of
    -- SelfDies rather than its generalisation. Two consequences follow, and both
    -- are what make the two constructors behave differently rather than one being
    -- a wider Filter over the other:
    --
    --   * no CR 603.10a look-back. That rule's list of exceptions covers
    --     leaves-the-battlefield abilities and this is not one, so CR 603.10's
    --     normal reading applies: the bearer is the object as it exists
    --     immediately AFTER the event, which is the CR 400.7 incarnation in the
    --     graveyard. SelfDies reads the departing incarnation instead.
    --   * CR 113.6k puts the ability in the graveyard. Since the trigger never
    --     fires with the bearer on the battlefield, the condition cannot trigger
    --     from there, so it functions in every zone it can trigger from -- which
    --     is what lets a Serra Avatar discarded out of a hand trigger at all.
    --
    -- SUPERSET of SelfPutIntoGraveyardFromLibrary above in what it matches, and
    -- still a separate constructor: Narcomoeba's printed sentence names one
    -- origin zone and must stay silent for a discard, so collapsing the two would
    -- be a rules divergence rather than a refactor.
    SelfPutIntoGraveyardFromAnywhere
  | -- | CR 603.6c, narrowed to the one written form Doomed Traveler prints -- CR
    -- 700.4's "dies", the battlefield-to-graveyard pair. NOT the whole of CR
    -- 603.6c, whose first clause reaches any zone at all; that is
    -- SelfLeavesTheBattlefield below, since a card saying "leaves the
    -- battlefield" must not be conflated with one saying "dies". Self-scoped like
    -- SelfEnters.
    --
    -- The bearer is the incarnation that was ON THE BATTLEFIELD -- the id
    -- ZoneChange.departed carries and GameState.lastKnown files under -- and NOT
    -- the CR 400.7 incarnation that arrived. That is CR 603.10a read through CR
    -- 603.10's own definition of looking back, and both halves live in that one
    -- last-known record: the ability exists because the pre-move projection says
    -- so, and CR 603.3a's controller is whoever controlled the permanent as it
    -- left. The arriving incarnation is not lost -- CR 400.7e lets the ability
    -- find it, and Pawl.Engine.Event.eventBindings binds that id under
    -- Pawl.Engine.Binding.became -- so the bearer and the object the payload acts
    -- on are deliberately two different ids.
    --
    -- The contrast with SelfPutIntoGraveyardFromLibrary above is exact: that one
    -- is library to graveyard, functions IN the graveyard (CR 113.6k), and reads
    -- the ARRIVING incarnation; this one is battlefield to graveyard, functions on
    -- the battlefield, and reads the DEPARTING one.
    SelfDies
  | -- | The SAME written form as SelfDies above, read by a BYSTANDER rather than by
    -- the permanent that died. Meren of Clan Nel Toth's "whenever another creature
    -- you control dies".
    --
    -- Two constructors for exactly PermanentEnters' and SelfEnters' reason: that
    -- one is a bare comparison of ids, while this one has to READ the dead
    -- permanent's characteristics, and reading them can come up empty for a
    -- permanent that ceased without last known information ever being filed.
    -- Nothing about the bearer is part of the match; the bearer is the
    -- Filter.Context's source, and its controller is CR 109.5's "you". Named for
    -- the rule, NOT "AnotherPermanentDies": "another" is
    -- Filter.Not Filter.IsSource inside the Filter (#163), and a card watching
    -- EVERY creature die is the same constructor with a wider Filter.
    --
    -- The candidate is ZoneChange.departed and NOT ZoneChange.object -- the
    -- permanent as it was on the battlefield, read from CR 608.2h last known
    -- information (CR 603.10a again). That is what makes "you control" answerable
    -- CORRECTLY: by the CR 117.5 boundary the candidate is a card in a graveyard,
    -- CR 108.4 gives it no controller, and CR 108.4a would hand back its OWNER --
    -- a different player for anything its controller had stolen.
    --
    -- The CANDIDATE and what the PAYLOAD acts on are therefore two different ids,
    -- as they are for SelfDies: CR 400.7e lets the ability find the graveyard
    -- incarnation, and Pawl.Engine.Event.eventBindings binds THAT one -- the
    -- ZoneChange.object -- under Pawl.Engine.Binding.became. Promise of Tomorrow's
    -- "whenever a creature you control dies, exile it" reads it.
    PermanentDies (Filter.Filter Keyword.Keyword)
  | -- | CR 603.6c's FIRST written form -- "when [this object] leaves the
    -- battlefield" -- taken whole: ANY other zone, so an exile, a bounce and a
    -- shuffle into a library all fire it. Thragtusk's.
    --
    -- SIBLING of SelfDies above, never its superset in code even though it is one
    -- in the rules. The two written forms are different printed sentences, and CR
    -- 700.4 narrows the second to a graveyard, so a Doomed Traveler must stay
    -- silent for exactly the bounce that fires a Thragtusk. Self-scoped and a
    -- LOOK-BACK for SelfDies' reasons (CR 603.10a): the bearer is
    -- ZoneChange.departed, and CR 603.3a's controller is whoever controlled it
    -- then.
    --
    -- Where it DIVERGES from SelfDies is CR 400.7e, whose rescue of the arriving
    -- object holds only for a public destination -- satisfied by construction for
    -- a graveyard (CR 400.2), but a real test here, since a hand or a library is
    -- hidden. Pawl.Engine.Event.eventBindings binds Pawl.Engine.Binding.became only
    -- for a public destination, and nothing at all for a hidden one.
    --
    -- CR 603.6c's SECOND trigger event -- a phased-in permanent leaving the game
    -- with its owner (CR 800.4a) -- is matched too, off GameEvent.LeftTheGame.
    -- It is the one form of this condition that fires without a zone change, and
    -- so the one whose bearer can only be read from CR 608.2h last known
    -- information: Pawl.DepartureSpec's Thragtusk case is the proof, and its
    -- phased-out pair is CR 702.26k's.
    SelfLeavesTheBattlefield
  | -- | CR 702.55b/702.55c: "when the creature this card haunts dies" -- the death
    -- half of Blind Hunter's printed ability, borne by the haunting CARD IN
    -- EXILE. CR 700.4's event again, so the match is PermanentDies' zone pair;
    -- WHICH permanent is the one GameState.haunting files the bearer against, the
    -- link Effect.ExileHaunting wrote as the card was exiled.
    --
    -- The only condition that cannot trigger from the battlefield for a reason
    -- that is about the BEARER rather than about the event: a permanent on the
    -- battlefield haunts nothing, so CR 113.6k sends this to the exile zone and
    -- Pawl.Engine.Event.zonesTriggeredFrom answers Zone.Exile. Rule 702.55c says
    -- as much in its own words.
    --
    -- NO payload and no Filter, unlike PermanentDies: rule 702.55b makes the
    -- haunted object "the object targeted by the haunt ability, regardless of
    -- whether or not that object is still a creature", so the link is the whole
    -- match and no characteristic of the deceased is read.
    HauntedCreatureDies
  | -- | CR 701.6a: "whenever a spell or ability you control counters a spell" --
    -- Baral, Chief of Compliance's. Matched against GameEvent.SpellCountered,
    -- whose Countering carries the controller of whatever DID the countering; the
    -- PlayerRelation reads that player against CR 109.5's "you", the ability's
    -- controller (CR 603.3a). NOT self-scoped -- PlayerDiscards' shape, not a
    -- Self- condition's -- since Baral is a creature watching an instant resolve.
    --
    -- The relation is on the COUNTERING side, never on the countered spell's: the
    -- printed sentence has one "you" and it modifies "a spell or ability", so
    -- Baral triggers on countering its own controller's spell just as readily.
    --
    -- Only a countered SPELL fires this, which is the printed word rather than an
    -- omission. Stifle counters an ABILITY and Baral stays silent -- CR 113.9 says
    -- an ability on the stack is not a spell, so Pawl.Engine.Event.counter ceases
    -- it (CR 608.2n) and records no event to match. The countered-ability sibling
    -- of that record does not exist, and no card in the pool wants one (#541).
    SpellOrAbilityCounters PlayerRelation.PlayerRelation
  | -- | CR 615.13: "whenever damage that would be dealt to you is prevented" --
    -- Selfless Squire's. Matched against GameEvent.DamagePrevented, whose
    -- Recipient is whom the prevented damage was addressed to; the PlayerRelation
    -- reads that player against CR 109.5's "you", the ability's controller (CR
    -- 603.3a). PlayerDiscards' shape, not a Self- condition's: the bearer is a
    -- creature watching damage addressed to its controller.
    --
    -- Scoped to a PLAYER recipient rather than any recipient, which is the
    -- printed sentence rather than an omission -- CR 615.13 itself says nothing
    -- about who the damage was addressed to, so a card watching damage to a
    -- CREATURE being prevented would be a different condition, and no card in the
    -- pool is one. It fires for damage prevented to a player and stays silent for
    -- damage prevented to a permanent.
    --
    -- NOT linked to the prevention effect that fired it. Selfless Squire's own
    -- ruling is explicit that any prevention at all triggers it, including one
    -- its own first ability had nothing to do with; a card printing "prevented
    -- this way" is what must add the link (#687).
    DamageToPlayerPrevented PlayerRelation.PlayerRelation
  | -- | CR 119.9: "whenever [a player] gains life" -- Ajani's Pridemate's. That
    -- rule rewrites the printed sentence as "whenever a SOURCE causes [a player]
    -- to gain life", which is why this is matched against GameEvent.LifeGained,
    -- recorded only where a source caused the gain, rather than against any
    -- upward movement of a life total. The PlayerRelation reads the gaining
    -- player against CR 109.5's "you", the ability's controller (CR 603.3a).
    --
    -- PlayerDiscards' shape, not a Self- condition's: the bearer is a creature
    -- watching its own controller's life total, and CR 119.9 says nothing about
    -- which object the ability is on.
    --
    -- ONE fire per recorded event, which is where CR 702.15e's "simultaneous
    -- lifelink sources cause separate life gain events" is honoured -- by
    -- Pawl.Engine.Damage recording one event per damage event rather than by
    -- anything here counting.
    --
    -- The amount is not part of the CONDITION -- CR 119.9 makes any gain greater
    -- than 0 match, whatever its size -- but it is part of the EVENT, which is why
    -- Pawl.Engine.Event.eventBindings binds it under
    -- Pawl.Engine.Binding.eventAmount for Sanguine Bond's "that much" to read.
    -- Ajani's Pridemate names no number and simply ignores the slot. The gaining
    -- PLAYER rides alongside it under Pawl.Engine.Binding.triggerPlayer for the
    -- same reason -- False Cure's "that player" is whoever the event named, which
    -- under AnyPlayer is not CR 109.5's "you".
    --
    -- LOSING life is not the negative of this condition but a different event
    -- entirely (GameEvent.LifeLost), so a card bearing this stays silent for a
    -- loss, for prevented damage (CR 615.6 -- the life never left), and for a
    -- gain by anyone the relation excludes. PlayerLosesLife below is that other
    -- event's condition, and the two are read by different cards.
    PlayerGainsLife PlayerRelation.PlayerRelation
  | -- | "Whenever [a player] loses life" -- Exquisite Blood's. PlayerGainsLife
    -- above is the mirror in shape, and matched against GameEvent.LifeLost; the
    -- PlayerRelation reads the LOSING player against CR 109.5's "you", the
    -- ability's controller (CR 603.3a). PlayerDiscards' shape, not a Self-
    -- condition's: the bearer is an enchantment watching a life total that is
    -- not its controller's.
    --
    -- No CR 119.9 for this direction. That rule rewrites "whenever [a player]
    -- gains life" as "whenever a SOURCE causes [a player] to gain life" and says
    -- a gain of 0 is no event, and the rules print no such sentence for loss --
    -- so what this condition matches is settled by what the engine RECORDS as a
    -- loss, and the recording sites are the citation. All three are life leaving
    -- a player by a rule that says so:
    --
    --   * CR 119.3, an effect that causes a player to lose life
    --     (Pawl.Engine.Resolve's LoseLife arm).
    --   * CR 119.2 / 120.3a, damage dealt to a player by a source without
    --     infect, which CAUSES that player to lose that much life
    --     (Pawl.Engine.Damage).
    --   * CR 119.4, life a cost or an effect has a player pay -- "in other
    --     words, the player loses that much life" (Pawl.Engine.Event.payLife).
    --
    -- Three life-total facts that are NOT this event, each for a rule's reason:
    -- CR 120.3b's infect damage gives poison counters INSTEAD of the life loss;
    -- CR 615.6's prevented damage never leaves the total (CR 615.13 is its own
    -- trigger event); and CR 120.3c through 120.3e take a permanent's damage
    -- somewhere no player's life total is.
    --
    -- CR 119.5's life-total SET would be a loss by that rule's own words --
    -- "the player gains or loses the necessary amount of life" -- whenever the
    -- new total is lower, but it has no producer in the pool and so records
    -- nothing for this to match.
    --
    -- The zero case needs no check here. Every producer above guards its own
    -- zero, so a GameEvent.LifeLost in the log is a loss of more than 0 by
    -- construction; with no CR 119.9 to cite, that is an invariant of the
    -- recording sites rather than a rule this condition restates.
    --
    -- ONE fire per recorded event, exactly as for PlayerGainsLife: a batch of
    -- simultaneous damage records one loss per damage event, and nothing here
    -- counts.
    --
    -- The amount is not part of the CONDITION -- any loss greater than 0 matches,
    -- whatever its size -- but it is part of the EVENT (CR 603.2), which is why
    -- Pawl.Engine.Event.eventBindings binds it under
    -- Pawl.Engine.Binding.eventAmount for Exquisite Blood's "that much" to read.
    --
    -- The LOSING player is part of the same event, and gets a slot of their own
    -- under Pawl.Engine.Binding.triggerPlayer -- the slot CR 701.9a's discard
    -- condition already stamps -- for Mindcrank's "that player mills that many
    -- cards" to read. Exquisite Blood's payload names only "you" and reads it
    -- not at all, which is why the amount arrived one card earlier.
    PlayerLosesLife PlayerRelation.PlayerRelation
  | -- | CR 714.2b, generalized over the kind of counter: "when one or more [kind]
    -- counters are put onto this permanent, if the number of [kind] counters on it
    -- was less than N and became at least N". A THRESHOLD CROSSING, matched
    -- against a GameEvent.CountersPut whose before/after pair straddles N.
    --
    -- Bearer-scoped, like SelfEnters: the event names an object and the match is a
    -- comparison of ids.
    --
    -- The WHOLE sentence, intervening "if" included, rather than the event half
    -- here and the "if" half in TriggeredAbility.intervening. Both of that
    -- clause's conjuncts describe the counter-placement event -- what the number
    -- WAS and what it BECAME -- and neither is a fact about the board that CR
    -- 603.4's second check could find changed at resolution. Splitting them would
    -- put half a sentence into a Condition that cannot express it anyway:
    -- Pawl.Types.Condition is one measured-comparison-threshold triple, and "was
    -- less than N and became at least N" is a conjunction of two.
    --
    -- The consequence is that a chapter ability already on the stack still
    -- resolves after its Saga's lore counters are removed, which is the rule: CR
    -- 704.5s's "isn't the source of a chapter ability that has triggered but not
    -- yet left the stack" exists precisely because such an ability is expected to
    -- be waiting there, and a clause that a removal could falsify would make that
    -- exemption unreachable.
    --
    -- The Natural is N, the chapter number. CR 714.2c's "{rN1}, {rN2}--[Effect]"
    -- is two abilities sharing one effect, and that is how a card writes it: two
    -- entries in triggeredAbilities with the same modal and different N. The rule
    -- says the shorthand MEANS that, so nothing here has to represent it.
    --
    -- Not restricted to Sagas, and not restricted to lore counters. CR 714.1 puts
    -- chapter symbols on Sagas, but the shape is the counter kind's rather than the
    -- subtype's, and Pawl.Engine.Saga is where the Saga-only rules (CR 714.2d's
    -- final chapter number, CR 714.3c's turn-based action, CR 704.5s's state-based
    -- action) read the subtype.
    SelfCountersReached SelfCountersReached.SelfCountersReached
  | -- | CR 310.12b, generalized over the kind of counter: "when the LAST [kind]
    -- counter is removed from this permanent". SelfCountersReached's mirror,
    -- matched against a GameEvent.CountersRemoved whose before/after pair went from
    -- one or more to none.
    --
    -- Bearer-scoped, like SelfCountersReached: the event names an object and the
    -- match is a comparison of ids.
    --
    -- "The last counter is removed" is a fact about the EVENT and not about the
    -- board, which is why the whole sentence lives here rather than half of it in
    -- TriggeredAbility.intervening -- SelfCountersReached's Haddock argues the same
    -- point at length. A permanent whose counters were removed and then replaced
    -- before the ability resolved still had its last counter removed.
    --
    -- Takes no threshold, because rule 310.12b states none: "the last" is
    -- after == 0, and a rule wanting "the last N" would be a different clause.
    --
    -- Not restricted to battles and not restricted to defense counters, for
    -- SelfCountersReached's reason: the shape is the counter kind's, and
    -- Pawl.Engine.Battle is where rule 310's battle-only reading lives.
    SelfLastCounterRemoved (CounterKind.CounterKind Keyword.Keyword)
  | -- | CR 601.2i: "whenever you cast a [type] spell" -- Young Pyromancer's
    -- "whenever you cast an instant or sorcery spell". That rule's second
    -- sentence is the trigger event in as many words: "any abilities that
    -- trigger when a spell is cast or put onto the stack trigger at this time".
    -- Matched against GameEvent.SpellCast, whose ObjectId is the spell.
    --
    -- PermanentEnters' shape rather than PlayerDiscards': a FILTER over the
    -- spell, not a PlayerRelation over the caster. The printed sentence narrows
    -- two things at once -- who cast it and what it was -- and only one of those
    -- is a player, so a PlayerRelation would say half of it and leave "an
    -- instant or sorcery" unwritable. Both halves fit in the Filter instead:
    -- "you cast" is Filter.ControlledBy You read against CR 109.5's "you", the
    -- ability's controller (CR 603.3a), and "an instant or sorcery spell" is a
    -- disjunction of Filter.HasCardType. A card watching an OPPONENT cast is the
    -- same constructor with Filter.ControlledBy Opponent, which is the second
    -- reason the relation is not a separate payload.
    --
    -- NOT self-scoped: the bearer is a creature watching the stack, and is the
    -- Filter.Context's source, so "another" is expressible as
    -- Filter.Not Filter.IsSource if a card ever wants it. "This spell" is
    -- SelfCast below rather than Filter.IsSource here, for the zone reason that
    -- constructor gives.
    --
    -- The spell is read AS IT IS ON THE STACK. CR 608.2h is not involved -- CR
    -- 601.2a leaves the spell there "until it resolves, it's countered, or a rule
    -- or effect moves it elsewhere", none of which can have happened yet -- but
    -- the stack incarnation is the right object for a stronger reason: CR 601.2i
    -- applies the effects that modify the spell's characteristics BEFORE it
    -- becomes cast, so a card in the hand is the wrong thing to read.
    --
    -- The spell itself is bound for the payload to name, under the reserved
    -- Pawl.Engine.Binding.castSpell slot: Presence of the Master's "counter it"
    -- is CR 701.6a acting on that slot. The CASTER is bound alongside it, under
    -- the reserved Pawl.Engine.Binding.triggerPlayer slot -- CR 112.2's
    -- controller of the spell, which Kambal, Consul of Allocation's "that player
    -- loses 2 life" names without going through the spell (CR 608.2h can have
    -- taken the spell away by resolution).
    --
    -- The TurnScope is a SECOND axis beside the Filter, and Brineborn
    -- Cutthroat's "whenever you cast a spell DURING AN OPPONENT'S TURN" is what
    -- earns it. Whose turn it is is not a characteristic of the spell, so it
    -- cannot be smuggled into the Filter -- Pawl.Types.Filter's atoms are all
    -- characteristics of a candidate -- and it is not carried by
    -- GameEvent.SpellCast either, so Event.matchesTrigger reads it off the
    -- GameState's active player. Young Pyromancer and the rest print no turn at
    -- all and take TurnScope.EachTurn, which is CR 601.2i's own silence: that
    -- rule says nothing about whose turn it is, and CR 117.1a lets an instant be
    -- cast on anybody's.
    --
    -- The same type StepBegins carries above, read against the same player: CR
    -- 109.5's "you", the ability's controller when it triggered (CR 603.3a).
    --
    -- The ORDINAL is a fourth axis, and Clarion Spirit's "whenever you cast your
    -- SECOND spell each turn" earns it. It is the same question
    -- PlayerDrawsNthCard asks of CR 121's draw, and it is a field of the payload
    -- rather than a condition of its own for that constructor's mirror-image
    -- reason: which cast of the turn it was narrows the very event the Filter,
    -- the scope and the zone already describe.
    SpellCast SpellCast.SpellCast
  | -- | CR 601.2i again, read off the spell BEING cast rather than off a bystander
    -- -- "when you cast this spell", Desolation Twin's. Self-scoped like
    -- SelfEnters, and a sibling of SpellCast above rather than
    -- @SpellCast Filter.IsSource@ for SelfEnters' and SelfDies' reason: the match
    -- is a bare comparison of ids, needing no projection of the spell at all.
    --
    -- The ZONE is the second reason, and the one this constructor exists for.
    -- CR 113.6k puts a trigger condition that can't trigger from the battlefield
    -- in the zones it can trigger from, and this one cannot: CR 601.2a moves the
    -- object to the stack to cast it and leaves it there, so it is on the stack
    -- and not on the battlefield at the moment CR 601.2i fires this.
    -- Pawl.Engine.Event.zonesTriggeredFrom answers that with a total case over
    -- this type; asking the same of a Filter's shape would be a partial analysis
    -- of an open language, silently answering "battlefield" for every shape it
    -- had not anticipated.
    --
    -- No TurnScope and no Filter: "this spell" is the whole subject, and no
    -- printing narrows its own cast by whose turn it is.
    SelfCast
  | -- | CR 601.2c: "whenever this permanent becomes the target of a spell or
    -- ability [a player] controls" -- CR 702.21a's ward. Matched against
    -- GameEvent.BecameTarget, whose `targeted` is compared with the bearer; the
    -- PlayerRelation reads the targeting object's controller against CR 109.5's
    -- "you", the ability's controller (CR 603.3a).
    --
    -- Self-scoped like SelfCast above, plus a relation: rule 702.21a's subject is
    -- the bearer itself, and its "an opponent controls" is the only part of the
    -- sentence a bystander form would generalise. A Filter over the targeting
    -- object would be the fuller shape and no printing in the pool wants one --
    -- the relation is what ward asks and all it asks.
    --
    -- On the STACK as well as on the battlefield, unlike SelfCast above: rule
    -- 601.2c's event is about the bearer being NAMED, so a spell on the stack
    -- becomes a target as readily as a permanent does. Ward is printed on
    -- permanents (rule 702.21a says "this permanent"), so no reader is on the
    -- stack today, and Pawl.Engine.Event.zonesTriggeredFrom answering the
    -- battlefield is what confines it.
    --
    -- Fires ONCE PER ANNOUNCEMENT of the bearer as a target, which is rule
    -- 601.2c's own arity: that rule lets one object be chosen once for each
    -- instance of the word "target", and each choice is a becoming. A spell that
    -- names the bearer once fires this once, however many other things it also
    -- targets.
    --
    -- No re-targeting site fires it, and CR 115.7's effects are why one would: an
    -- effect that changes a target makes the new object become a target too. No
    -- such effect is in the pool (#1525).
    SelfBecomesTargeted PlayerRelation.PlayerRelation
  | -- | CR 709.5h: "when you unlock this door" -- fires when the permanent bearing
    -- the ability is given the unlocked designation for the NAMED half. "Some
    -- abilities trigger when a player unlocks a particular half of a permanent.
    -- These abilities trigger when that permanent is given the appropriate
    -- unlocked designation, regardless of whether it was given that designation
    -- while entering the battlefield or after entering the battlefield."
    --
    -- Self-scoped like SelfEnters above -- the bearer's identity is part of the
    -- match -- plus the half, which is what makes this the only condition that
    -- names one. CR 709.5h's "a PARTICULAR half" is why: a Room whose two doors
    -- both carry an unlock trigger has two abilities that see the same event, and
    -- only the door's name separates them. The name is the ability's own half,
    -- which is what "this door" means (CR 709.5j: "A door is a half of that
    -- permanent") -- Pawl.CardSpec's lint holds that it is one of the card's own
    -- face names.
    --
    -- Reaching the half from the ABILITY instead would need the face an ability
    -- was printed on to survive into Pawl.Engine.Projection's flattened list, and
    -- it does not: CR 709.4c combines the halves' abilities into one text box and
    -- nothing downstream remembers which half wrote which line. Naming the door
    -- is the card stating a fact about itself, the same kind of thing a chosen
    -- name is, and not the engine casing on which card it is.
    --
    -- The condition is about the DESIGNATION and not about who gave it: a door
    -- unlocked by CR 709.5e's special action, by CR 709.5f's keyword action, and
    -- by CR 709.5d's entry all fire it. CR 709.5i's "fully unlocks" is a second
    -- shape, and RoomFullyUnlocked below is it.
    SelfHalfUnlocked CardName.CardName
  | -- | CR 709.5i: "Some abilities trigger when a player 'fully unlocks' a
    -- permanent with a shared type line. Such an ability triggers when that
    -- permanent has one of the two unlocked designations and gets the other, or
    -- when it has neither designation and gains both."
    --
    -- NOT self-scoped, and that is the whole difference from SelfHalfUnlocked
    -- above: Balemurk Leech watches every Room on the board rather than its own
    -- doors, so the bearer only supplies the perspective. The PlayerRelation reads
    -- the permanent's CONTROLLER against CR 109.5's "you".
    --
    -- Which is a reading of the printed "YOU fully unlock", and not the same
    -- sentence: the rule's subject is the player taking the action, while this
    -- asks who controls the permanent. GameEvent.HalfUnlocked carries no actor,
    -- and CR 709.5f's keyword action reaches Pawl.Engine.Event.unlockHalf with no
    -- payer at all, so the actor is not available to ask about (#961).
    --
    -- Names no half, unlike SelfHalfUnlocked: CR 709.5i is about the permanent
    -- becoming fully unlocked, which is a fact about ALL of its halves and so
    -- about none of them in particular.
    RoomFullyUnlocked PlayerRelation.PlayerRelation
  | -- | Several conditions, any of which fires the ONE ability that bears them.
    --
    -- CR 603.1b's own shape: "a triggered ability may have MORE THAN ONE trigger
    -- condition". Balemurk Leech is the pool's: "Eerie -- Whenever an enchantment
    -- you control enters and whenever you fully unlock a Room, each opponent loses
    -- 1 life." Eerie is an ABILITY WORD (CR 207.2c: ability words "have no special
    -- rules meaning and no individual entries in the Comprehensive Rules"), so it
    -- grants nothing and is not modelled; what is left is one ability with two
    -- trigger conditions.
    --
    -- ONE ability rather than two, which is observable: CR 603.8's suppression,
    -- CR 603.3b's ordering and CR 603.1b's own "all of those conditions" clause
    -- all count abilities, and printing two would count wrong.
    --
    -- CR 603.1b's "all of those conditions have happened during a particular
    -- period" is a SECOND thing a multi-condition ability can do, and nothing here
    -- does it: this list is read as "any", which is what a card joining its
    -- clauses with "and whenever" means. No printing in the pool carries the
    -- "all" instruction.
    --
    -- Pawl.CardSpec's lint forbids a StateIs or a nested AnyOf inside one. The
    -- first because CR 603.8's state triggers and CR 603.2's event triggers are
    -- gathered by different scans (Pawl.Engine.Event.stateTriggers against
    -- matchesTrigger), and an ability that was both would be gathered by neither
    -- coherently; the second because a flat list says everything a nested one
    -- could.
    AnyOf [TriggerCondition]
  | -- | CR 708.7 through CR 603.2: "when this creature is turned face up". Fires
    -- when the permanent BEARING the ability is the one that turned over, which
    -- is SelfEnters' shape and for its reason -- a bare comparison of ids, with
    -- nothing about the permanent's characteristics read, so there is no CR
    -- 608.2h fallback to reach for.
    --
    -- SELF-scoped. CR 603.6a's second written form has a counterpart here --
    -- "whenever a permanent is turned face up", borne by some other permanent
    -- entirely -- and that is PermanentTurnedFaceUp below. Skirk Marauder is the
    -- printing this one answers.
    --
    -- No PAYLOAD. CR 702.37e's morph cost, the player who took the special
    -- action and the characteristics the permanent regained are all things a
    -- printed ability could in principle say "that much" about, and none does.
    SelfTurnedFaceUp
  | -- | The SAME rule read by a BYSTANDER: CR 708.7's other written form,
    -- "whenever a permanent is turned face up", borne by a permanent that is NOT
    -- the one turning over. Aven Farseer's.
    --
    -- Two constructors for exactly PermanentEnters' and SelfEnters' reason, and
    -- PermanentDies' and SelfDies': that one is a bare comparison of ids, while
    -- this one has to READ the permanent's characteristics to answer a narrowed
    -- form. Nothing about the bearer is part of the match; the bearer is the
    -- Filter.Context's source, and its controller is CR 109.5's "you" in
    -- Deathmist Raptor's "a permanent YOU CONTROL is turned face up".
    --
    -- FILTERED rather than payload-free even though Aven Farseer says only "a
    -- permanent", which is Filter's trivial `And []`: the narrowed printings
    -- exist (Deathmist Raptor, Hamza), and "another" would be
    -- Filter.Not Filter.IsSource inside the Filter (#163) rather than a third
    -- constructor.
    --
    -- A LIVE read, unlike PermanentDies': CR 708.8 leaves the permanent on the
    -- battlefield with its normal copiable values back, so CR 603.10a's list of
    -- look-back exceptions does not reach this condition and CR 603.10's first
    -- sentence governs. The Filter reads the permanent as it is AFTER the
    -- turning, which is what makes "a creature is turned face up" answerable at
    -- all -- a face-down permanent has only the characteristics its listing
    -- names (CR 708.2), never the card's, so reading it before would answer
    -- every narrowed form wrong.
    PermanentTurnedFaceUp (Filter.Filter Keyword.Keyword)
  | -- | A permanent the Filter admits GAINED THIS DESIGNATION -- Valeron Wardens'
    -- "whenever a creature you control becomes renowned" (CR 702.112b) and Arbor
    -- Colossus' "when this creature becomes monstrous" (CR 701.37b). Read at the
    -- moment the marker is given, matched against GameEvent.BecameDesignated.
    --
    -- The designation is a PAYLOAD beside the Filter, for Pawl.Types.Designation's
    -- reason, and it is what keeps the two readings apart: Valeron Wardens must not
    -- draw when a creature you control becomes monstrous.
    --
    -- PermanentTurnedFaceUp's shape exactly, and for its reasons: one FILTERED
    -- constructor rather than a self-scoped pair, the bearer entering only as the
    -- Filter.Context's source and CR 109.5's "you". Relic Seeker's "when THIS
    -- creature becomes renowned" and Arbor Colossus' monstrous trigger are this
    -- condition with Filter.IsSource, so the self form needs no constructor of its
    -- own.
    --
    -- A LIVE read of the permanent, PermanentTurnedFaceUp's posture: nothing here
    -- is a zone change, so CR 603.10a's look-back does not reach it and the
    -- designation is written while the permanent is still on the battlefield.
    PermanentBecomesDesignated PermanentBecomesDesignated.PermanentBecomesDesignated
  | -- | CR 702.100b: the BEARER evolved -- Renegade Krasis' "whenever this
    -- creature evolves". Matched against GameEvent.Evolved by an id comparison,
    -- SelfEnters' shape.
    --
    -- Self-scoped and NOT PermanentBecomesDesignated's filtered shape, and the pool
    -- is why: both printings that read rule 702.100b's marker say "this creature",
    -- where renown has Valeron Wardens printing the filtered form. A Filter
    -- payload here would be a width no card asks for; the card that prints
    -- "whenever a creature you control evolves" earns it.
    SelfEvolves
  | -- | CR 702.134c: the creature the bearer is ATTACHED TO mentored another
    -- creature -- Aegis of the Legion's "whenever equipped creature mentors a
    -- creature". Matched against GameEvent.Mentored, whose first id is the mentor
    -- and whose second Pawl.Engine.Event.eventBindings binds under
    -- Pawl.Engine.Binding.mentoredCreature for the payload's "that creature".
    --
    -- Rule 702.134c's own reading of the event, and nothing is left for this arm to
    -- check beyond WHO mentored: the rule fires on "a mentor ability whose source is
    -- the first creature and whose target is the second creature" resolving, so the
    -- pairing is the event's and not a comparison to redo here.
    --
    -- ATTACHMENT-scoped rather than self-scoped or filtered, and the pool is why:
    -- the one printing that reads rule 702.134c is an Equipment, so its source is
    -- never the mentoring creature (SelfEvolves' shape would ask about the Aegis)
    -- and "equipped creature" is not a class of objects a Filter could name (CR
    -- 301.5f makes it whatever creature this source is attached to). The same
    -- sentence Affected.Attached states for a static ability, stated for a trigger.
    -- A card printing "whenever a creature you control mentors" earns the filtered
    -- form, and one printing "whenever this creature mentors" the self-scoped one.
    --
    -- Vacuously False while the source is attached to nothing, or to a player (CR
    -- 303.4's other destination): an unattached Equipment has no equipped creature,
    -- so nothing it could watch has mentored.
    AttachedCreatureMentors
  | -- | CR 702.149c: the BEARER trained -- Savior of Ollenbock's "whenever this
    -- creature trains". Matched against GameEvent.Trained by an id comparison,
    -- SelfEvolves' shape exactly.
    --
    -- Self-scoped and not AttachedCreatureMentors' attachment scope, because rule
    -- 702.149c states the self form in as many words ("when THIS creature
    -- trains") and its one printing is the training creature itself. A card
    -- printing "whenever a creature you control trains" earns the filtered form.
    --
    -- Nothing is left for this arm to check beyond WHOSE training it was: the
    -- event is recorded only when a resolving training ability actually put a
    -- counter on, which is rule 702.149c's whole condition.
    SelfTrains
  | -- | CR 603.10a: "whenever a player sacrifices a permanent". One of the four
    -- look-back families that rule names, and the second pawl builds.
    --
    -- NOT the same condition as SelfDies or PermanentDies, even though CR 700.4
    -- makes every sacrifice a death: this one fires on the sacrifice AS a
    -- sacrifice (CR 701.21a's game action), which the zone change alone cannot
    -- say. GameEvent.PermanentSacrificed is the event, recorded beside the Moved
    -- event rather than instead of it, so a death trigger still sees a sacrifice
    -- and this one does not see a destruction.
    --
    -- NO PAYLOAD, and the omission is deliberate. Mayhem Devil says "whenever a
    -- PLAYER sacrifices a permanent" -- any player, its own controller included --
    -- which is exactly PlayerRelation.AnyPlayer, so a relation here would carry
    -- that one value on every printing in the pool and say nothing. The card that
    -- prints "whenever you sacrifice" is still the one that earns it. Nothing
    -- about the sacrificed PERMANENT is asked either: Mayhem Devil says "a
    -- permanent", so there is no Filter to carry.
    --
    -- Not self-scoped: the bearer watches every sacrifice on the board, so it
    -- contributes neither an identity nor CR 109.5's perspective.
    PermanentSacrificed
  | -- | CR 603.3b's own second class: a trigger condition that IS another ability
    -- triggering. "Whenever the final chapter ability of a Saga you control
    -- triggers" -- Historian's Boon's, and the only printed condition of this
    -- shape.
    --
    -- Matched against GameEvent.AbilityTriggered, which Pawl.Engine.Engine
    -- appends for each gathered trigger before the batch goes on the stack. That
    -- is what makes this condition observable at all: every other condition here
    -- describes something that happened to the board, and this one describes
    -- something the rules did.
    --
    -- Being in this class is the whole reason CR 603.3b has two passes, and
    -- Pawl.Engine.Event.reactsToAbilityTriggering is where that classification
    -- lives -- exhaustively, so a new condition must decide which pass it takes
    -- rather than defaulting into the first.
    --
    -- THREE things are checked together, and none of them is separable into a
    -- Filter over the source: the ability must be a CHAPTER ability (CR 714.2b's
    -- condition), its chapter must be its source's FINAL chapter number (CR
    -- 714.2d), and the source must be a Saga with chapter abilities (CR 714.1 /
    -- 704.5s's "Saga permanent with one or more chapter abilities"). The middle
    -- one is a comparison between the event and the source's projection, which no
    -- Filter atom can express.
    --
    -- The PlayerRelation reads the TRIGGERED ability's controller (CR 603.3a,
    -- carried on the event) against CR 109.5's "you", the watching ability's
    -- controller. PlayerDiscards' shape, not a Self- condition's: the bearer is
    -- an enchantment watching a Saga that is a different permanent entirely.
    --
    -- No payload for WHICH Saga or WHICH chapter. Historian's Boon's effect names
    -- neither, so Pawl.Engine.Event.eventBindingSlots answers empty; a card
    -- printing "that Saga" is what would earn a slot.
    SagaFinalChapterTriggers PlayerRelation.PlayerRelation
  | -- | CR 725.1: "whenever [a player] becomes the monarch" -- Custodi Lich's.
    -- Matched against GameEvent.BecameMonarch, whose PlayerId is the newly
    -- crowned player; the PlayerRelation reads that player against CR 109.5's
    -- "you", the ability's controller (CR 603.3a). PlayerDiscards' shape, not a
    -- Self- condition's: the bearer is a creature watching a DESIGNATION change
    -- and contributes only the perspective the relation is read from.
    --
    -- ONE constructor with a relation rather than two, for PlayerDiscards' and
    -- PlayerGainsLife's reason: "you become the monarch" and "an opponent
    -- becomes the monarch" are the same trigger EVENT read from two seats, not
    -- two events. CR 725.1 states the event once.
    --
    -- No Filter, and that is CR 725.3 rather than an omission: "Only one player
    -- can be the monarch at a time", so a crowning names exactly one player and
    -- there is nothing to select among. A relation is the whole of what a card
    -- can say about who that player is.
    --
    -- Matched against the EVENT, never against how the crown was won: an entry
    -- trigger's crown (Palace Jailer), a targeted crown (Denethor, Stone Seer)
    -- and CR 725.2's stolen crown all record the same event, so the relation is
    -- the only thing that decides whether this fires. TriggerSpec's "CR 725.2 a
    -- stolen crown is a crowning, and fires the same trigger" is the test that
    -- proves the last of those, which is the route with no card in it at all.
    --
    -- CR 725.4's departure reassignment does not reach this yet (#1052).
    PlayerBecomesMonarch PlayerRelation.PlayerRelation
  | -- | CR 603.7: "when you lose control of the creature" -- Ray of Command's third
    -- sentence. Fires when the permanent BOUND IN THE NAMED SLOT stops being
    -- controlled by "you", matched against GameEvent.ControlChanged.
    --
    -- The only condition that names a SLOT, because it is the only one whose
    -- subject is a particular object chosen earlier rather than the bearer or a
    -- class of objects. A delayed ability's source is the SPELL that armed it (CR
    -- 603.7d), so a Self- condition would ask about Ray of Command on the stack
    -- and never about the creature; and a Filter would ask about "a creature",
    -- which is a different sentence -- the printed one is about THE creature, the
    -- one the spell untapped. CR 603.7c's captured environment is what remembers
    -- which, so the condition reads the same binding the ability's own effect does.
    --
    -- "You" is the ability's controller: the player who controlled the spell as it
    -- resolved (CR 603.7d), matched against the player the event says control LEFT.
    -- Where it went is not read, because the printed sentence does not ask.
    --
    -- An empty slot, or a slot holding something that is not a permanent, never
    -- matches. That is CR 608.2b's fizzle already having happened rather than a
    -- guard: a spell whose only target became illegal does not resolve, so it arms
    -- nothing.
    --
    -- A permanent LEAVING the battlefield is not a match, because
    -- GameEvent.ControlChanged is sampled on the battlefield only. You do lose
    -- control of a creature that dies, so the entry stays armed instead of
    -- spending CR 603.7b's one shot -- unobservable, since the ability could only
    -- act on an object CR 400.7 has already replaced with a new one.
    LoseControlOfBound SlotName.SlotName
  | -- | CR 309.4c: "When you move your venture marker into this room" -- the
    -- trigger condition every room ability has and no dungeon card prints. Fires
    -- against GameEvent.VentureMarkerEntered naming the bearer and this room.
    --
    -- Self-scoped through the bearer AND the room index, SelfHalfUnlocked's shape
    -- and for its reason: a dungeon has one ability per room, all of them borne by
    -- the same card, and only the room separates them. "This room" is what the
    -- index names.
    --
    -- Never written by card data -- Pawl.Codec.TriggerCondition round-trips it like
    -- any other, but no dungeon prints it. Pawl.Engine.Dungeon mints one per room
    -- of Pawl.Types.Face.rooms, which is why Pawl.Types.DungeonRoom carries a bare
    -- Modal rather than a whole TriggeredAbility.
    RoomEntered RoomIndex.RoomIndex
  | -- | CR 701.22d: "whenever you scry" -- Matoya, Archon Elder's first branch.
    -- Matched against GameEvent.Scried, whose PlayerId is the player who
    -- scried; the PlayerRelation reads that player against CR 109.5's "you",
    -- the ability's controller (CR 603.3a). PlayerBecomesMonarch's shape: the
    -- bearer contributes only the seat the relation is read from, and
    -- "whenever an opponent scries" is the same event read from the other one.
    --
    -- Counts SCRIES rather than cards, which is the whole reason it reads its
    -- own event: CR 701.22a's reorder moves cards inside one library and
    -- records nothing, and a scry that moved nothing still fires this
    -- (CR 701.22d). CR 701.22b's scry 0 records no event and so fires nothing.
    PlayerScries PlayerRelation.PlayerRelation
  | -- | CR 701.25d, PlayerScries' twin and Matoya, Archon Elder's second
    -- branch. Matched against GameEvent.Surveiled, and a surveil that put
    -- nothing into a graveyard fires it just the same -- which is what keeps
    -- this apart from a condition built on the zone changes CR 701.25a's
    -- graveyard half records. CR 701.25c's surveil 0 fires nothing.
    PlayerSurveils PlayerRelation.PlayerRelation
  | -- | CR 702.170e: "when this card becomes plotted" -- Aloe Alchemist's.
    -- Matched against GameEvent.Plotted naming the bearer.
    --
    -- SELF-scoped and nullary, SelfCycled's shape: the ability is printed on
    -- the card that becomes plotted, and CR 702.170a functions it in the hand,
    -- so no bystander form is in print. It is watched for from EXILE, where CR
    -- 702.170b's special action leaves the card -- Pawl.Engine.Event's
    -- zonesTriggeredFrom is where that is said.
    SelfBecomesPlotted
  | -- | CR 701.44b: "whenever a creature you control explores" -- Wildgrowth
    -- Walker's. Matched against GameEvent.Explored, with the Filter applied to
    -- the EXPLORER rather than to the bearer -- PermanentDies' posture, and
    -- through last known information for that arm's reason and CR 701.44c's.
    --
    -- Fires once per completed explore, including one whose library was empty
    -- (CR 701.44b), which is what keeps it apart from a condition built on the
    -- reveal, the counter or the bin CR 701.44a's steps record.
    PermanentExplores (Filter.Filter Keyword.Keyword)
  | -- | CR 701.43d \/ 607.2h: "when you do" beside "you may exert this creature
    -- as it attacks" -- Glory-Bound Initiate's. Matched against
    -- GameEvent.Exerted by an id comparison, SelfEvolves' shape.
    --
    -- SELF-scoped and nullary because CR 701.43d states the linked form and
    -- nothing else: the trigger is printed in the same paragraph as the static
    -- ability, so its subject is always the bearer. A card printing "whenever a
    -- creature you control is exerted" would earn the filtered form, and none
    -- is in print.
    --
    -- CR 607.2h's linkage holds by construction rather than by a link field:
    -- the event is recorded only by the CR 508.1g payment on THIS permanent, so
    -- "actions taken as a result of the static ability" is the only thing this
    -- can match. The one gap that leaves is a card bearing two exert paragraphs,
    -- whose two triggers would each see both exerts -- CR 701.43d makes that a
    -- second static ability, and nothing in print has one.
    SelfExerted
  deriving (Eq, Ord, Show)
