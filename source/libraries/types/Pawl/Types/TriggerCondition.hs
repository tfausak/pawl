module Pawl.Types.TriggerCondition where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
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
  | -- | CR 603.6a read by a bystander: "whenever a [type] enters", the bearer
    -- included.
    PermanentEnters (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2b: "at the beginning of [each|your] <step>"; the TurnScope
    -- decides whose turn qualifies.
    StepBegins StepBegins.StepBegins
  | -- | CR 603.8: a state trigger, armed again only once the ability has left
    -- the stack.
    StateIs Condition.Condition
  | -- | CR 603.2 / 509-510: the bearer dealt combat damage to a player.
    -- Self-scoped.
    SelfDealsCombatDamageToPlayer
  | -- | CR 120.3: the bearer was dealt damage -- the enrage trigger's event
    -- (Ripjaw Raptor).
    SelfIsDealtDamage
  | -- | CR 603.2 / 509-510 read by a bystander: a permanent the Filter admits
    -- dealt combat damage to a player (Tovolar, Dire Overlord).
    PermanentDealsCombatDamageToPlayer (Filter.Filter Keyword.Keyword)
  | -- | CR 725.2: a creature dealt combat damage to the monarch. Borne by no
    -- card; matched only via Pawl.Engine.Monarch.inherentMatch.
    CreatureDealtCombatDamageToMonarch
  | -- | CR 702.179d: "whenever one or more opponents lose life during your
    -- turn". Borne by no card; matched via Pawl.Engine.Speed.inherentPending.
    OpponentLostLifeDuringYourTurn
  | -- | CR 702.29c: "when you cycle this card". Self-scoped.
    SelfCycled
  | -- | CR 702.94a: "when you reveal this card this way" -- miracle's triggered
    -- half, linked (CR 603.11) to the static half. Self-scoped.
    SelfRevealedForMiracle
  | -- | CR 701.9a: "when you discard this card" (Bartered Cow). Self-scoped.
    SelfDiscarded
  | -- | CR 701.9a read by a bystander: "whenever [a player] discards a card"
    -- (Megrim).
    PlayerDiscards PlayerRelation.PlayerRelation
  | -- | "Whenever you cycle a card" (Prickly Marmoset) -- PlayerDiscards
    -- narrowed to Pawl.Types.DiscardCause.ToPayCyclingCost by CR 702.29a.
    PlayerCycles PlayerRelation.PlayerRelation
  | -- | CR 121.1: "whenever [a player] draws their Nth card each turn" (Erudite
    -- Wizard), by equality on the ordinal the event carries.
    PlayerDrawsNthCard PlayerDrawsNthCard.PlayerDrawsNthCard
  | -- | CR 508.3a: "whenever [a creature] attacks" (Hanweir Garrison).
    -- Self-scoped; the TriggerFrequency is Aurelia, the Warleader's "for the
    -- first time each turn".
    SelfAttacks TriggerFrequency.TriggerFrequency
  | -- | CR 508.3a with a companion required -- rule 702.149a's training, the
    -- Filter asked existentially of everybody else the declaration named.
    SelfAttacksWithAnother (Filter.Filter Keyword.Keyword)
  | -- | CR 506.5 read by a bystander: "whenever a creature you control attacks
    -- alone" -- rule 702.83a's exalted.
    CreatureAttacksAlone (Filter.Filter Keyword.Keyword)
  | -- | CR 508.3a's second sentence read by a bystander: "whenever a creature
    -- attacks you or a planeswalker you control" (Marchesa's Decree), once per
    -- declared attacker.
    CreatureAttacksYou
  | -- | CR 508.3b: "whenever enchanted player is attacked" (Curse of Vitality),
    -- once per distinct target, the subject read off Object.attachedTo.
    --
    -- Not implemented: rule 508.3b's planeswalker and battle subjects --
    -- Scryfall o:"is attacked" o:"whenever", 2026-08-21, matches five cards and
    -- all five are Curses enchanting a player, and GameEvent.BecameAttacked
    -- already carries the permanent an arm for them would read (#2279).
    AttachedPlayerIsAttacked
  | -- | CR 508.3d: "whenever [a player] attacks" -- once per declaration,
    -- against GameEvent.AttackersDeclared, with the declaring player bound
    -- under Pawl.Engine.Binding.attackingPlayer.
    PlayerAttacks PlayerRelation.PlayerRelation
  | -- | CR 508.3c: "whenever [a player] attacks with [a creature]" (Hermes,
    -- Overseer of Elpis) -- the arm above narrowed by a Filter over the
    -- creatures declared.
    --
    -- Not implemented: a floor above one, which Aurelia, the Law Above's "with
    -- three or more creatures" needs (#2226).
    PlayerAttacksWith PlayerAttacksWith.PlayerAttacksWith
  | -- | CR 508.3e: "whenever [a player] attacks [another player]" (Seifer,
    -- Balamb Rival), once per pair; only AttackTarget.OfPlayer matches. Both
    -- players are bound -- see Pawl.Engine.Event.eventBindingSlots.
    PlayerAttacksPlayer PlayerAttacksPlayer.PlayerAttacksPlayer
  | -- | CR 702.105a: dethrone -- SelfAttacks narrowed to the player with the
    -- most life, or tied for most. Only AttackTarget.OfPlayer satisfies it.
    SelfAttacksPlayerWithMostLife
  | -- | CR 509.3a: "whenever [a creature] blocks" (Pride Guardian).
    -- Self-scoped, and once per blocking creature however many it blocked.
    SelfBlocks
  | -- | CR 509.3b: "whenever [a creature] blocks a creature" (Loyal Sentry) --
    -- SelfBlocks per attacker blocked, with the Filter over that attacker.
    --
    -- Not implemented: rule 509.3b's other producer, an effect that causes the
    -- bearer to block, records no event (#1146).
    SelfBlocksCreature (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e: "whenever [a creature] blocks two or more creatures"
    -- (Lairwatch Giant); the Natural is a floor, never an exact count.
    SelfBlocksAtLeast Natural.Natural
  | -- | CR 509.3e: "whenever [a creature] blocks one or more [F] creatures"
    -- (Serra Inquisitors' first half) -- SelfBlocksAtLeast spending the number
    -- on a quality instead.
    --
    -- Not implemented: rule 509.3e's "effects that add or remove blockers" reach
    -- neither this nor SelfBlocksAtLeast (#1146).
    SelfBlocksOneOrMore (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3c: "whenever [a creature] becomes blocked" (Sacred Prey) -- the
    -- attacking side of SelfBlocks, once per attacker that got a blocker.
    --
    -- Rule 509.3c's other two producers reach it: Effect.BecomesBlocked (Curtain
    -- of Light) and a creature put onto the battlefield blocking (CR 509.4),
    -- which Flash Foliage exercises -- Pawl.CombatEffectSpec's
    -- PutOntoBattlefieldBlocking group is the proof.
    SelfBecomesBlocked
  | -- | CR 509.3d: "whenever [a creature] becomes blocked by a creature" -- rule
    -- 702.25a's flanking, once for each creature that blocks.
    --
    -- A creature put onto the battlefield blocking fires it too, unlike
    -- SelfBlocksCreature: Flash Foliage blocking Benalish Cavalry is the pooled
    -- pair, and Pawl.CombatEffectSpec's PutOntoBattlefieldBlocking group is the
    -- proof.
    --
    -- Not implemented: rule 509.3d's remaining producer, an effect that causes a
    -- creature to block, records no event (#1146).
    SelfBecomesBlockedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e: "whenever [a creature] becomes blocked by one or more [F]
    -- creatures" (Serra Inquisitors' second half) -- SelfBlocksOneOrMore from
    -- the attacking side, so the whole declaration fires it once.
    --
    -- Rule 509.3e's "effects that add or remove blockers" reaches it where the
    -- arrival is the first admitted blocker: Aetherplasm swapping itself out for
    -- a black creature card is the pooled pair, and Pawl.KeywordTriggerSpec's
    -- SelfBlocksOneOrMore group is the proof.
    --
    -- Not implemented: an effect that causes a creature already on the
    -- battlefield to block records no event at all (#1146).
    SelfBecomesBlockedByOneOrMore (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e read by a bystander on the attacking side: "whenever a
    -- creature attacking one of your opponents becomes blocked by two or more
    -- creatures" (Seifer, Balamb Rival); the number is a floor, and only
    -- Pawl.Types.AttackTarget.OfPlayer satisfies the PlayerRelation.
    --
    -- Both GameEvent.AttackerBlocked and GameEvent.BecameBlocking bind the
    -- attacker, which Pawl.ZoneTriggerSpec's representativeEvents pins by
    -- listing both. Rule 509.3e's added blockers reach it through a creature put
    -- onto the battlefield blocking an already-blocked attacker: Flash Foliage's
    -- Saproling joining a declared Hill Giant is the pooled pair, and
    -- Pawl.KeywordTriggerSpec's CreatureBecomesBlockedByAtLeast group is the
    -- proof.
    --
    -- Not implemented: an effect that causes a creature already on the
    -- battlefield to block, which records no event (#1146).
    CreatureBecomesBlockedByAtLeast CreatureBecomesBlockedByAtLeast.CreatureBecomesBlockedByAtLeast
  | -- | CR 509.1h: "whenever this creature attacks and isn't blocked" -- CR
    -- 702.68a's frenzy, with the status fixed at the declaration.
    SelfAttacksUnblocked
  | -- | CR 603.6: "when this card is put into your graveyard from your library"
    -- (Narcomoeba). Self-scoped, and the bearer is CR 400.7's new incarnation.
    SelfPutIntoGraveyardFromLibrary
  | -- | CR 603.6: "when this card is put into a graveyard from anywhere" (Serra
    -- Avatar). Self-scoped, and not a leaves-the-battlefield ability (CR 603.6c).
    SelfPutIntoGraveyardFromAnywhere
  | -- | CR 603.6c narrowed to CR 700.4's "dies", the battlefield-to-graveyard
    -- pair (Doomed Traveler). Self-scoped, and a CR 603.10a look-back.
    SelfDies
  | -- | The same written form read by a bystander (Meren of Clan Nel Toth),
    -- filtered over the departed permanent's last known information.
    PermanentDies (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2c's batch reading of the same form: "whenever one or more other
    -- creatures you control die" (Vengeful Townsfolk), once for the batch.
    PermanentsDie (Filter.Filter Keyword.Keyword)
  | -- | CR 603.6c's first written form taken whole -- "when [this object] leaves
    -- the battlefield" (Thragtusk) -- plus that rule's and CR 729.4a's
    -- leaving-the-game forms. Self-scoped and a look-back.
    SelfLeavesTheBattlefield
  | -- | The same written form read by a bystander (Super Shredder), filtered
    -- over the departed permanent's last known information.
    PermanentLeavesTheBattlefield (Filter.Filter Keyword.Keyword)
  | -- | CR 603.6c's family narrowed to one destination: "whenever another
    -- nonland permanent you control is returned to its owner's hand" (Justice,
    -- Vance Astrovik). A look-back, CR 603.10a naming this form too.
    --
    -- Not implemented: CR 603.2c's batch reading of the same event -- "whenever
    -- one or more noncreature permanents are returned to hand" (Tameshi, Reality
    -- Architect), which fires once however many moved, the way PermanentsDie
    -- stands beside PermanentDies (#2682).
    PermanentReturnedToHand (Filter.Filter Keyword.Keyword)
  | -- | CR 700.4's "dies" read off the permanent the bearer is attached to
    -- (Screams from Within); the one condition CR 113.6m's Aura clause names.
    AttachedCreatureDies
  | -- | CR 701.26a's "became tapped" read off the permanent the bearer is
    -- attached to (Betrayal), live rather than through last known information.
    AttachedCreatureBecomesTapped
  | -- | CR 702.55b / 702.55c: "when the creature this card haunts dies", borne
    -- by the haunting card in exile.
    HauntedCreatureDies
  | -- | CR 701.6a: "whenever a spell or ability you control counters a spell"
    -- (Baral, Chief of Compliance); the relation is on the countering side.
    --
    -- Not implemented: a countered-ability event and its condition (#541).
    SpellOrAbilityCounters PlayerRelation.PlayerRelation
  | -- | CR 615.13: "whenever damage that would be dealt to you is prevented"
    -- (Selfless Squire), blind to which prevention effect applied.
    DamageToPlayerPrevented PlayerRelation.PlayerRelation
  | -- | CR 615.13 the other way round: "when damage is prevented this way"
    -- (Phyrexian Vindicator), matching where CR 614.5's applying instance names
    -- the bearer as its source; the Filter is over CR 120.1's damage source.
    SelfPreventsDamage (Filter.Filter Keyword.Keyword)
  | -- | CR 119.9: "whenever [a player] gains life" (Ajani's Pridemate), once per
    -- gain a source caused.
    PlayerGainsLife PlayerRelation.PlayerRelation
  | -- | CR 603.2c's batch reading of the arm above: "whenever one or more
    -- players gain life", once for the whole CR 608.2f event.
    PlayersGainLife PlayerRelation.PlayerRelation
  | -- | "Whenever [a player] loses life" (Exquisite Blood), against
    -- GameEvent.LifeLost -- PlayerGainsLife's mirror in shape.
    --
    -- Not implemented: CR 119.5's life-total set, which would be a loss by that
    -- rule's own words whenever the new total is lower, records nothing.
    PlayerLosesLife PlayerRelation.PlayerRelation
  | -- | CR 714.2b generalized over the counter kind: a threshold crossing whose
    -- before/after pair straddles N, intervening "if" included.
    SelfCountersReached SelfCountersReached.SelfCountersReached
  | -- | CR 716.2a: "when this Class becomes level N" (Stormchaser's Talent), a
    -- crossing rather than an equality.
    SelfBecomesClassLevel ClassLevel.ClassLevel
  | -- | CR 310.12b generalized over the counter kind: "when the last [kind]
    -- counter is removed from this permanent".
    SelfLastCounterRemoved (CounterKind.CounterKind Keyword.Keyword)
  | -- | "Whenever one or more [kind] counters are removed from this permanent"
    -- (Chandra, Fire Artisan) -- the arm above with no reading of the after
    -- count.
    SelfCountersRemoved (CounterKind.CounterKind Keyword.Keyword)
  | -- | CR 603.2c's batch reading of a CR 122.6 placement: "whenever one or more
    -- [kind] counters are put on one or more [permanents]", once for the batch.
    PermanentsGetCounters CounterPlacement.CounterPlacement
  | -- | The arm above read per permanent: "whenever one or more [kind] counters
    -- are put on a [permanent]" (Wickersmith's Tools).
    --
    -- Not implemented: a slot for the permanent this condition names, which
    -- Auntie Ool, Cursewretch's "if you control that creature" reads (#2342).
    PermanentGetsCounters CounterPlacement.CounterPlacement
  | -- | CR 601.2i: "whenever you cast a [type] spell" (Young Pyromancer), the
    -- Filter read against the spell as it is on the stack.
    SpellCast SpellCast.SpellCast
  | -- | CR 601.2i read off the spell being cast -- "when you cast this spell"
    -- (Desolation Twin). Self-scoped, which is what lets
    -- Pawl.Engine.Event.zonesTriggeredFrom answer CR 113.6k totally.
    SelfCast
  | -- | CR 601.2c: "whenever this permanent becomes the target of a spell or
    -- ability [a player] controls" -- CR 702.21a's ward, once per instance of
    -- the word "target".
    --
    -- Not implemented: CR 115.7's re-targeting effects, which would make a new
    -- object become a target (#1525).
    SelfBecomesTargeted PlayerRelation.PlayerRelation
  | -- | CR 601.2c from the player's side: "whenever you become the target of a
    -- spell or ability" (Dormant Gomazoa).
    ControllerBecomesTarget ControllerBecomesTarget.ControllerBecomesTarget
  | -- | CR 709.5h: "when you unlock this door", however the named half was
    -- unlocked. Self-scoped plus the half, which is what separates a Room's two
    -- doors.
    SelfHalfUnlocked CardName.CardName
  | -- | CR 709.5i: the permanent gained the unlocked designation it lacked, or
    -- gained both. Not self-scoped (Balemurk Leech).
    --
    -- The PlayerRelation reads the player who unlocked, proved by Pawl.RoomSpec's
    -- "CR 709.5i 'you' is the player who unlocked, not the Room's controller".
    RoomFullyUnlocked PlayerRelation.PlayerRelation
  | -- | CR 603.1b: several conditions, any of which fires the one ability that
    -- bears them (Balemurk Leech); Pawl.CardSpec's lint forbids a StateIs or a
    -- nested AnyOf inside one.
    AnyOf [TriggerCondition]
  | -- | CR 708.7 through CR 603.2: "when this creature is turned face up" (Skirk
    -- Marauder). Self-scoped.
    SelfTurnedFaceUp
  | -- | CR 701.27e: "when this creature transforms into [face]", matched against
    -- the names the event carries. Self-scoped plus the name, which is what
    -- tells the turn to a face from the turn away from it.
    --
    -- Not implemented: the bystander form CR 701.27e also admits, a card
    -- watching another permanent transform (Corruption of Towashi, Neglected
    -- Heirloom). The event carries everything such a condition would read, so
    -- what is missing is the arm rather than the record (#2050).
    SelfTransformedInto CardName.CardName
  | -- | CR 708.7's other written form read by a bystander (Aven Farseer),
    -- filtered and read live after CR 708.8 restores the copiable values.
    PermanentTurnedFaceUp (Filter.Filter Keyword.Keyword)
  | -- | A permanent the Filter admits gained this designation -- CR 702.112b's
    -- renown (Valeron Wardens) and CR 701.37b's monstrous (Arbor Colossus).
    PermanentBecomesDesignated PermanentBecomesDesignated.PermanentBecomesDesignated
  | -- | CR 702.100b: the bearer evolved (Renegade Krasis). Self-scoped.
    SelfEvolves
  | -- | CR 702.134c: the creature the bearer is attached to mentored another
    -- (Aegis of the Legion). Attachment-scoped, so vacuously False while
    -- attached to nothing or to a player (CR 303.4).
    AttachedCreatureMentors
  | -- | CR 702.149c: the bearer trained (Savior of Ollenbock). Self-scoped, and
    -- recorded only where a counter actually went on.
    SelfTrains
  | -- | CR 603.10a: "whenever a player sacrifices a permanent" (Mayhem Devil) --
    -- the CR 701.21a game action, not the zone change SelfDies reads.
    PermanentSacrificed
  | -- | CR 603.3b's second class: "whenever the final chapter ability of a Saga
    -- you control triggers" (Historian's Boon), against
    -- GameEvent.AbilityTriggered.
    SagaFinalChapterTriggers PlayerRelation.PlayerRelation
  | -- | CR 725.1: "whenever [a player] becomes the monarch" (Custodi Lich),
    -- however the crown was won.
    PlayerBecomesMonarch PlayerRelation.PlayerRelation
  | -- | CR 603.7: "when you lose control of the creature" (Ray of Command) --
    -- the only condition naming a slot, its subject being an object CR 603.7c's
    -- captured environment chose earlier.
    LoseControlOfBound SlotName.SlotName
  | -- | CR 309.4c: "when you move your venture marker into this room".
    -- Self-scoped through the bearer and the room index; minted by
    -- Pawl.Engine.Dungeon rather than written by card data.
    RoomEntered RoomIndex.RoomIndex
  | -- | CR 309.7: "whenever you complete a dungeon" (Dungeon Crawler), against
    -- GameEvent.DungeonCompleted.
    PlayerCompletesDungeon PlayerRelation.PlayerRelation
  | -- | CR 701.22d: "whenever you scry" (Matoya, Archon Elder). Counts scries
    -- rather than cards; CR 701.22b's scry 0 records no event.
    PlayerScries PlayerRelation.PlayerRelation
  | -- | CR 701.25d, PlayerScries' twin: a surveil that put nothing into a
    -- graveyard fires it just the same, and CR 701.25c's surveil 0 fires nothing.
    PlayerSurveils PlayerRelation.PlayerRelation
  | -- | CR 706.1: "whenever you roll one or more dice" (Feywild Trickster). The
    -- event and not the result, which is what lets CR 706.7's planar die fire it.
    --
    -- Not implemented: the printed "one or more", which needs CR 706.1's die
    -- count (#2085); one roll is one event, so the batch and per-occurrence
    -- readings coincide today. See #934 for the planar die.
    PlayerRollsDice PlayerRelation.PlayerRelation
  | -- | CR 705.2: "whenever you win a coin flip" (Tavern Scoundrel), reading the
    -- event's win where PlayerRollsDice ignores what the die showed.
    --
    -- Not implemented: an outcome-blind "whenever you flip a coin", and a losing
    -- one, each of which would be a sibling arm over the same event (gap #2306).
    PlayerWinsCoinFlip PlayerRelation.PlayerRelation
  | -- | CR 702.170a / 702.170c: "when this card becomes plotted" (Aloe
    -- Alchemist). Self-scoped, and watched for from exile.
    SelfBecomesPlotted
  | -- | CR 701.44b: "whenever a creature you control explores" (Wildgrowth
    -- Walker), once per completed explore including one whose library was empty.
    PermanentExplores (Filter.Filter Keyword.Keyword)
  | -- | CR 701.43d \/ 607.2h: "when you do" beside "you may exert this creature
    -- as it attacks" (Glory-Bound Initiate). Self-scoped, the linkage holding by
    -- construction.
    --
    -- Not implemented: a card bearing two exert paragraphs, whose two triggers
    -- would each see both exerts.
    SelfExerted
  | -- | CR 701.3a read by the host: "whenever an Aura becomes attached to this
    -- creature" (Bramble Elemental), with the Filter over the attachment.
    --
    -- Not implemented: the other scope, the bearer as the attachment (Enormous
    -- Energy Blade), which needs its own constructor and a binding for "that
    -- creature" (gap #1837).
    SelfBecomesAttachedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 603.12's reflexive triggered ability: "when you do" (The Fugitive
    -- Doctor). Nullary, and it matches no GameEvent -- the arming clause runs
    -- exactly when the action it hangs off was taken, so
    -- Pawl.Engine.Event.delayedPending fires it once at the next gather.
    --
    -- Not implemented: CR 603.12a's first sentence, "once for each of those
    -- times", which rule 603.12's other printed form ("when [something happens]
    -- this way") reaches -- that event is no payment and can occur several times
    -- in one resolution, where this fires once (#2121).
    Reflexive
  deriving (Eq, Ord, Show)
