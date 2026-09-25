module Pawl.Types.TriggerCondition where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AbilityAddsMana as AbilityAddsMana
import qualified Pawl.Types.CardLeavesZone as CardLeavesZone
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Types.DieResult as DieResult
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.Types.PermanentsBecomeTargeted as PermanentsBecomeTargeted
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
  | -- | CR 603.2 / 509-510: the bearer dealt combat damage to a player the
    -- PlayerRelation admits. Self-scoped on the SOURCE, which is what "self"
    -- names; the relation is the RECIPIENT half, read against CR 109.5's "you".
    --
    -- The relation is written out rather than defaulted, PermanentSacrificed's
    -- posture: Longtusk Cub's "deals combat damage to a player" spells itself
    -- AnyPlayer, where Questing Beast's "to an opponent" is Opponent, and the two
    -- are different triggers on a board where the damage is redirected to the
    -- creature's own controller (CR 614.9).
    SelfDealsCombatDamageToPlayer PlayerRelation.PlayerRelation
  | -- | CR 603.2 / 120.1: the bearer dealt damage of ANY kind to a player the
    -- PlayerRelation admits -- the arm above without CR 510.1's combat narrowing.
    -- Akki Lavarunner's "whenever this creature deals damage to an opponent" is
    -- the printed form, and Soul's Fire aiming it at a player is the noncombat
    -- half no combat-only arm reaches.
    SelfDealsDamageToPlayer PlayerRelation.PlayerRelation
  | -- | CR 603.2 / 120.3: the bearer dealt damage of any kind to a CREATURE --
    -- the arm above with rule 120.3's other recipient (Strax, Sontaran Nurse's
    -- "Glory of Battle").
    --
    -- Nullary rather than Filter-carrying: the printed form qualifies the
    -- recipient no further than "a creature", and a Filter would owe CR 608.2h
    -- last known information for a recipient the damage has already killed.
    SelfDealsDamageToCreature
  | -- | CR 120.3: the bearer was dealt damage -- the enrage trigger's event
    -- (Ripjaw Raptor).
    SelfIsDealtDamage
  | -- | CR 603.2 / 509-510 read by a bystander: a permanent the Filter admits
    -- dealt combat damage to a player (Tovolar, Dire Overlord).
    PermanentDealsCombatDamageToPlayer (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2c's batch reading of the arm above: "whenever one or more
    -- artifact creatures you control deal combat damage to a player" (Pia
    -- Nalaar, Chief Mechanic), once for the whole CR 510.2 step.
    PermanentsDealCombatDamageToPlayer (Filter.Filter Keyword.Keyword)
  | -- | CR 725.2: a creature dealt combat damage to the monarch. Borne by no
    -- card; matched only via Pawl.Engine.Monarch.inherentMatch.
    CreatureDealtCombatDamageToMonarch
  | -- | CR 726.2: one or more creatures a player controls dealt combat damage to
    -- the player who has the initiative. Borne by no card; matched only via
    -- Pawl.Engine.Initiative.inherentPending.
    CreaturesDealtCombatDamageToInitiative
  | -- | CR 726.2: a player took the initiative. Borne by no card; matched only
    -- via Pawl.Engine.Initiative.inherentPending.
    PlayerTookInitiative
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
  | -- | CR 702.35a: "when this card is exiled this way" -- madness's triggered
    -- half, linked (CR 603.11) to the static half that exiled it. Self-scoped.
    SelfExiledForMadness
  | -- | CR 701.9a read by a bystander: "whenever [a player] discards a card"
    -- (Megrim).
    PlayerDiscards PlayerRelation.PlayerRelation
  | -- | CR 603.2c's batch reading of the arm above: "whenever you discard one or
    -- more cards" (Magmakin Artillerist), once per discard event.
    PlayerDiscardsCards PlayerRelation.PlayerRelation
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
  | -- | CR 508.3a's second sentence, self-scoped: "whenever this creature attacks
    -- a battle" (Thrashing Frontliner), the Filter asked of the PERMANENT CR
    -- 508.1b announced. AttackTarget.OfPlayer never satisfies it.
    SelfAttacksPermanent (Filter.Filter Keyword.Keyword)
  | -- | CR 506.5 read by a bystander: "whenever a creature you control attacks
    -- alone" -- rule 702.83a's exalted.
    CreatureAttacksAlone (Filter.Filter Keyword.Keyword)
  | -- | CR 508.3a's second sentence read by a bystander: "whenever a creature
    -- attacks you or a planeswalker you control" (Marchesa's Decree), once per
    -- declared attacker.
    CreatureAttacksYou
  | -- | CR 508.3a read by a bystander: "whenever a creature you control attacks"
    -- (Fervent Charge), once per declared attacker the Filter admits.
    CreatureAttacks (Filter.Filter Keyword.Keyword)
  | -- | CR 508.3b: "whenever enchanted player is attacked" (Curse of Vitality),
    -- once per distinct target, the subject read off Object.attachedTo.
    --
    -- Rule 508.3b's planeswalker and battle subjects are SelfIsAttacked below;
    -- Scryfall o:"is attacked" o:"whenever", 2026-09-07, still matches only
    -- these five Curses, so that arm's producer is synthetic.
    AttachedPlayerIsAttacked
  | -- | CR 508.3b's other two subjects: "whenever this planeswalker is attacked"
    -- (Synthetic Warded Sentinel), once per distinct target, the subject being
    -- the ability's own source rather than what it is attached to.
    SelfIsAttacked
  | -- | CR 508.3d: "whenever [a player] attacks" -- once per declaration,
    -- against GameEvent.AttackersDeclared, with the declaring player bound
    -- under Pawl.Engine.Binding.attackingPlayer.
    PlayerAttacks PlayerRelation.PlayerRelation
  | -- | CR 508.3c: "whenever [a player] attacks with [n or more creatures]"
    -- (Military Intelligence) -- the arm above narrowed by a Filter over the
    -- creatures declared and by how many of them the declaration names.
    PlayerAttacksWith PlayerAttacksWith.PlayerAttacksWith
  | -- | CR 508.3e: "whenever [a player] attacks [another player]" (Seifer,
    -- Balamb Rival), once per pair; only AttackTarget.OfPlayer matches. Both
    -- players are bound -- see Pawl.Engine.Event.Binding.eventBindingSlots.
    PlayerAttacksPlayer PlayerAttacksPlayer.PlayerAttacksPlayer
  | -- | CR 702.105a: dethrone -- SelfAttacks narrowed to the player with the
    -- most life, or tied for most. Only AttackTarget.OfPlayer satisfies it.
    SelfAttacksPlayerWithMostLife
  | -- | CR 702.171b: "whenever this creature attacks while saddled" (Bridled
    -- Bighorn) -- SelfAttacks narrowed by the bearer carrying
    -- Designation.Saddled as the declaration happens. The narrowing belongs to
    -- the event and not to TriggeredAbility.intervening: CR 603.4 would re-check
    -- an intervening "if" on resolution, where this clause asks only what was
    -- true as the creature attacked.
    SelfAttacksWhileSaddled
  | -- | CR 508.3a narrowed by a state that holds as the bearer attacks -- Kari
    -- Zev, Crew of Two\'s "whenever Kari Zev attacks while you don\'t control a
    -- legendary Monkey". SelfAttacksWhileSaddled\'s reading and for its reason:
    -- the event\'s own qualifier, never re-checked as TriggeredAbility.intervening
    -- would be (CR 603.4).
    SelfAttacksWhile Condition.Condition
  | -- | CR 509.3a: "whenever [a creature] blocks" (Pride Guardian).
    -- Self-scoped, and once per blocking creature however many it blocked.
    SelfBlocks
  | -- | CR 509.3b: "whenever [a creature] blocks a creature" (Loyal Sentry) --
    -- SelfBlocks per attacker blocked, with the Filter over that attacker.
    --
    -- Rule 509.3b's other producer reaches it: an effect that causes the bearer
    -- to block (Pawl.Engine.Combat.switchBlockers, General Jarkeld), which
    -- records the same event with the flag clear. Pawl.CombatCostSpec's
    -- SwitchBlockers group is the proof.
    SelfBlocksCreature (Filter.Filter Keyword.Keyword)
  | -- | CR 509.3e: "whenever [a creature] blocks two or more creatures"
    -- (Lairwatch Giant); the Natural is a floor, never an exact count.
    SelfBlocksAtLeast Natural.Natural
  | -- | CR 509.3e: "whenever [a creature] blocks one or more [F] creatures"
    -- (Serra Inquisitors' first half) -- SelfBlocksAtLeast spending the number
    -- on a quality instead.
    --
    -- Not implemented: rule 509.3e's "effects that add or remove blockers" reach
    -- neither this nor SelfBlocksAtLeast (#1146). The pool's one effect that
    -- makes an already-blocking creature block (General Jarkeld) is not that
    -- producer either: it moves a blocker between two attackers, leaving the
    -- number of creatures each blocker blocks exactly where it was.
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
    -- Rule 509.3d's remaining producer reaches it too, and through the same arm:
    -- an effect that causes a creature to block (General Jarkeld), which carries
    -- that rule's own guard, "only if it wasn't already blocking that attacking
    -- creature at that time". Pawl.CombatCostSpec's "CR 509.3d: but the Cavalry's
    -- flanking fired on the block the effect made" is the proof.
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
    -- battlefield to block (General Jarkeld) records GameEvent.BecameBlocking
    -- with putOntoBattlefield CLEAR, and this arm guards on that flag being set,
    -- so it misses that road (#1146).
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
    -- battlefield to block (General Jarkeld) records GameEvent.BecameBlocking
    -- with putOntoBattlefield CLEAR, and this arm guards on that flag being set,
    -- so it misses that road (#1146).
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
  | -- | CR 702.55a's second sentence: "when this spell is put into a graveyard
    -- during its resolution" (Cry of Contrition), which CR 608.2n makes the last
    -- step of an instant or sorcery spell's own resolution. Narrower than the arm
    -- above by CAUSE: a countered spell (CR 701.6a) and a fizzled one (CR 608.2b)
    -- reach the same graveyard from the same zone and do not match.
    SelfPutIntoGraveyardDuringResolution
  | -- | CR 603.6 read by a BYSTANDER: "whenever another card is put into a
    -- graveyard from anywhere" (Planar Void), filtered over the arriving card.
    -- CR 712.21's Example makes this the condition a melded permanent's death
    -- fires TWICE.
    CardPutIntoGraveyard (Filter.Filter Keyword.Keyword)
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
    PermanentReturnedToHand (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2c's batch reading of the arm above: "whenever one or more
    -- noncreature permanents are returned to hand" (Tameshi, Reality Architect),
    -- once however many moved, the way PermanentsDie stands beside PermanentDies.
    PermanentsReturnedToHand (Filter.Filter Keyword.Keyword)
  | -- | CR 400.7: a card leaves a named zone, per card -- "whenever a card leaves
    -- your graveyard during your turn" (Kishla Skimmer).
    CardLeavesZone CardLeavesZone.CardLeavesZone
  | -- | CR 603.2c's batch reading of the arm above: "whenever one or more cards
    -- leave your graveyard" (Spirit Mascot), once however many left.
    CardsLeaveZone CardLeavesZone.CardLeavesZone
  | -- | CR 700.4's "dies" read off the permanent the bearer is attached to
    -- (Screams from Within); the one condition CR 113.6m's Aura clause names.
    AttachedCreatureDies
  | -- | CR 701.26a's "became tapped" read off the permanent the bearer is
    -- attached to (Betrayal), live rather than through last known information.
    AttachedCreatureBecomesTapped
  | -- | CR 701.26a's batch reading, by a bystander: "whenever one or more
    -- nontoken Merfolk you control become tapped" (Deeproot Pilgrimage), once
    -- per tapping event (CR 603.2c).
    PermanentsBecomeTapped (Filter.Filter Keyword.Keyword)
  | -- | CR 701.26b's "becomes untapped" read off the bearer itself (Oreskos Sun
    -- Guide), the other direction of the status the arm above watches.
    SelfBecomesUntapped
  | -- | CR 106.12a's "is tapped for mana" read off the permanent the bearer is
    -- attached to (Wild Growth), live rather than through last known
    -- information, as for the arm above.
    AttachedPermanentTappedForMana
  | -- | CR 106.12a read by a bystander: "whenever you tap a land creature for
    -- mana" (Autumn Willow, Harmony), the arm above's event under a relation and
    -- a filter instead of an attachment link.
    PermanentTappedForMana PermanentTappedForMana.PermanentTappedForMana
  | -- | CR 605.1b's "mana being added to a player's mana pool": "whenever a
    -- land's ability causes you to add one or more mana of the chosen color"
    -- (Caged Sun).
    AbilityAddsMana AbilityAddsMana.AbilityAddsMana
  | -- | CR 605.1b's first alternative, self-scoped: "whenever a mana ability of
    -- this creature resolves" (Tyvar the Bellicose), against
    -- GameEvent.ManaAbilityResolved, with the mana the activation produced bound
    -- under Pawl.Engine.Binding.eventAmount.
    --
    -- RESOLUTION and not that rule's other half, a mana ability being ACTIVATED:
    -- every printing of the activation form states the negative instead
    -- (Rings of Brighthearth's "if it isn't a mana ability"), so nothing reads
    -- the moment between CR 605.3b's activation and the resolution it says
    -- follows immediately -- Scryfall o:/[Ww]henever.*mana abilit/, 2026-09-13,
    -- no hit of the positive shape. A card printing one would want a second arm
    -- here and a second event, not a widening of this one.
    SelfManaAbilityResolves
  | -- | CR 702.55b / 702.55c: "when the creature this card haunts dies", borne
    -- by the haunting card in exile.
    HauntedCreatureDies
  | -- | CR 701.6a: "whenever a spell or ability you control counters a spell"
    -- (Baral, Chief of Compliance); the relation is on the countering side.
    SpellOrAbilityCounters PlayerRelation.PlayerRelation
  | -- | CR 701.6a read from the VICTIM's side, and only for that rule's ability
    -- half: "whenever an ability is countered" (Synthetic Echo Silencer). Matches
    -- GameEvent.AbilityCountered and nothing else, so the arm above stays silent
    -- here and this one stays silent for a countered spell.
    AbilityIsCountered
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
    -- are put on a [permanent]" (Wickersmith's Tools). Naming one permanent, it
    -- binds it -- Auntie Ool, Cursewretch's "that creature", under CR 400.7e's
    -- `became` (Pawl.Engine.Event.Binding.eventBindingSlots) -- where the batch arm above
    -- cannot.
    PermanentGetsCounters CounterPlacement.CounterPlacement
  | -- | CR 601.2i: "whenever you cast a [type] spell" (Young Pyromancer), the
    -- Filter read against the spell as it is on the stack.
    SpellCast SpellCast.SpellCast
  | -- | CR 601.2i read off the spell being cast -- "when you cast this spell"
    -- (Desolation Twin). Self-scoped, which is what lets
    -- Pawl.Engine.Event.Trigger.zonesTriggeredFrom answer CR 113.6k totally.
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
  | -- | CR 603.2c's batch reading of the same rule read by a BYSTANDER:
    -- "whenever one or more creatures you control become the target of an
    -- activated ability" (Professor Hojo), once for the whole announcement.
    PermanentsBecomeTargeted PermanentsBecomeTargeted.PermanentsBecomeTargeted
  | -- | CR 601.2c per permanent: "whenever a creature you control becomes the
    -- target of a spell" (Venerated Rotpriest), once per creature targeted.
    PermanentBecomesTargeted PermanentsBecomeTargeted.PermanentsBecomeTargeted
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
    -- PermanentTransforms below is the same event read by a bystander -- the
    -- pair SelfTurnedFaceUp and PermanentTurnedFaceUp make one rule over.
    SelfTransformedInto CardName.CardName
  | -- | CR 701.27e read by a bystander: a permanent the Filter admits turned over
    -- (Cult of the Waxing Moon), matched against what the event SAMPLED rather
    -- than against the board the CR 117.5 scan reads.
    PermanentTransforms (Filter.Filter Keyword.Keyword)
  | -- | CR 708.7's other written form read by a bystander (Aven Farseer),
    -- filtered and read live after CR 708.8 restores the copiable values.
    PermanentTurnedFaceUp (Filter.Filter Keyword.Keyword)
  | -- | CR 701.27b: a permanent the Filter admits was turned face down (Synthetic
    -- Veiled Witness), read live after the turning.
    PermanentTurnedFaceDown (Filter.Filter Keyword.Keyword)
  | -- | A permanent the Filter admits gained this designation -- CR 702.112b's
    -- renown (Valeron Wardens) and CR 701.37b's monstrous (Arbor Colossus).
    PermanentBecomesDesignated PermanentBecomesDesignated.PermanentBecomesDesignated
  | -- | CR 702.100b: the bearer evolved (Renegade Krasis). Self-scoped.
    SelfEvolves
  | -- | CR 702.140d: "whenever this creature mutates" (Cubwarden), which rule
    -- 702.140d fires when a mutating creature spell merges with it. Self-scoped,
    -- SelfEvolves' shape: the merged permanent is the only object the event
    -- names.
    SelfMutates
  | -- | CR 702.134c: the creature the bearer is attached to mentored another
    -- (Aegis of the Legion). Attachment-scoped, so vacuously False while
    -- attached to nothing or to a player (CR 303.4).
    AttachedCreatureMentors
  | -- | CR 702.149c: the bearer trained (Savior of Ollenbock). Self-scoped, and
    -- recorded only where a counter actually went on.
    SelfTrains
  | -- | CR 702.110b: the bearer exploited a creature (Qarsi Sadist). Self-scoped,
    -- AttachedCreatureMentors' shape: rule 702.110b's "a creature" is a second
    -- object, bound under Pawl.Engine.Binding.exploitedCreature.
    -- Pawl.CardTriggerSpec's "CR 702.110b Profaner of the Dead bounces the
    -- creatures under the exploited creature's toughness" is what proves that slot
    -- is readable.
    SelfExploits
  | -- | CR 702.122e: "whenever this Vehicle becomes crewed" (Mobilizer Mech),
    -- which that rule defines as a crew ability of the bearer RESOLVING.
    -- Self-scoped; the TriggerFrequency is Mighty Servant of Leuk-o's "for the
    -- first time each turn", SelfAttacks' payload one rule over.
    --
    -- Rule 702.122e's second sentence is read off Pawl.Engine.Binding.crewers,
    -- which Pawl.Engine.Event.Binding stamps for every match: an intervening
    -- "if" counts that slot rather than the board, so it means only the
    -- creatures that paid the cost of the activation that caused the trigger.
    -- Pawl.CrewSpec's "CR 702.122e a second crewing this turn does not trigger
    -- it again" and "crewed by exactly two" are what prove it.
    SelfBecomesCrewed TriggerFrequency.TriggerFrequency
  | -- | CR 702.122b: "whenever this creature crews a Vehicle" (Gearshift Ace),
    -- which that rule makes true of a creature tapped to pay a Vehicle's crew
    -- cost. Self-scoped, SelfBecomesCrewed's other side: that one is asked of the
    -- Vehicle and this one of a crewer.
    SelfCrewsVehicle
  | -- | CR 603.10a: "whenever an opponent sacrifices an artifact" (Vengeful
    -- Tracker) -- the CR 701.21a game action, not the zone change SelfDies
    -- reads. Mayhem Devil's unrestricted wording is AnyPlayer and the trivial
    -- Filter.
    PermanentSacrificed PermanentSacrificed.PermanentSacrificed
  | -- | CR 603.3b's second class: "whenever the final chapter ability of a Saga
    -- you control triggers" (Historian's Boon), against
    -- GameEvent.AbilityTriggered.
    SagaFinalChapterTriggers PlayerRelation.PlayerRelation
  | -- | CR 725.1: "whenever [a player] becomes the monarch" (Custodi Lich),
    -- however the crown was won.
    PlayerBecomesMonarch PlayerRelation.PlayerRelation
  | -- | CR 603.7: "when you lose control of the creature" (Ray of Command) --
    -- the first of the three conditions naming a slot, its subject being an
    -- object CR 603.7c's captured environment chose earlier.
    LoseControlOfBound SlotName.SlotName
  | -- | CR 701.66a: the object bound at this slot is put into a graveyard or
    -- into exile from the battlefield -- "when that land dies or is put into
    -- exile", the far end of earthbend's delayed ability. Minted by
    -- Pawl.Engine.Earthbend rather than written by card data, and the second
    -- condition after LoseControlOfBound above to name a slot, its subject being
    -- the object CR 603.7c's captured environment chose earlier.
    BoundDiesOrIsExiled SlotName.SlotName
  | -- | CR 700.4: the object bound at this slot is put into a graveyard from the
    -- battlefield -- "when the creature dies this turn" (Whippoorwill), the arm
    -- above narrowed to the one destination rule 700.4 names.
    BoundDies SlotName.SlotName
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
    -- The printed "one or more" is the whole of one instruction's throw:
    -- Pawl.Engine.Resolve records one GameEvent.DiceRolled per throw, however
    -- many dice it named, and a reroll is a separate later throw, so the batch
    -- and per-occurrence readings coincide. See #934 for the planar die.
    PlayerRollsDice PlayerRelation.PlayerRelation
  | -- | CR 706.2: "whenever you roll a 6" (Night Shift of the Living Dead), once
    -- per die whose result, after every modifier, is the stated number.
    PlayerRollsResult (DieResult.DieResult PlayerRelation.PlayerRelation)
  | -- | CR 705.2: "whenever you win a coin flip" (Tavern Scoundrel), reading the
    -- event's win where PlayerRollsDice ignores what the die showed.
    --
    -- No third, outcome-blind arm: Scryfall o:"whenever you flip a coin" with
    -- include_extras, 2026-09-16, returns nothing, so no printing watches the flip
    -- without reading rule 705.2's outcome.
    PlayerWinsCoinFlip PlayerRelation.PlayerRelation
  | -- | CR 705.2: "whenever you lose a coin flip" (Karplusan Minotaur), the
    -- mirror of PlayerWinsCoinFlip over the same event.
    --
    -- The two are not exhaustive. CR 705.2's first sentence describes a flip
    -- nobody wins or loses, which both answer False to -- proved by
    -- Pawl.ReplacementSpec's "CR 705.2 nobody loses Molten Sentry's flip either".
    PlayerLosesCoinFlip PlayerRelation.PlayerRelation
  | -- | CR 702.170a / 702.170c: "when this card becomes plotted" (Aloe
    -- Alchemist). Self-scoped, and watched for from exile.
    SelfBecomesPlotted
  | -- | CR 701.44b: "whenever a creature you control explores" (Wildgrowth
    -- Walker), once per completed explore including one whose library was empty.
    PermanentExplores (Filter.Filter Keyword.Keyword)
  | -- | CR 701.50f: "whenever a creature you control connives" (Iron Monger,
    -- Sadistic Tycoon), once per completed connive. CR 701.50e is why a connive
    -- 0 does not reach it.
    PermanentConnives (Filter.Filter Keyword.Keyword)
  | -- | CR 701.43d \/ 607.2h: "when you do" beside "you may exert this creature
    -- as it attacks" (Glory-Bound Initiate). Self-scoped, the linkage holding by
    -- construction.
    --
    -- Not implemented: a card bearing two exert paragraphs, whose two triggers
    -- would each see both exerts.
    SelfExerted
  | -- | CR 701.3a read by the host: "whenever an Aura becomes attached to this
    -- creature" (Bramble Elemental), with the Filter over the attachment.
    SelfBecomesAttachedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 701.3a read by the attachment: "whenever this Equipment becomes
    -- attached to a creature" (Enormous Energy Blade), with the Filter over the
    -- host and the host bound as "that creature".
    SelfBecomesAttachedTo (Filter.Filter Keyword.Keyword)
  | -- | CR 701.3d read by the attachment: "whenever this Equipment becomes
    -- unattached from a permanent" (Grafted Wargear), with the Filter over the
    -- former host and that host bound as "that permanent".
    SelfBecomesUnattachedFrom (Filter.Filter Keyword.Keyword)
  | -- | CR 603.12's reflexive triggered ability: "when you do" (The Fugitive
    -- Doctor). Nullary, and it matches no GameEvent -- the arm runs only when
    -- the action it hangs off actually happened, CR 701.28e's ignored convert
    -- being the one that does not (Pawl.Engine.Resolve.Effect.applyClauseEffects), so
    -- Pawl.Engine.Event.Trigger.delayedPending fires it once at the next gather.
    --
    -- Not implemented: CR 603.12a's first sentence, "once for each of those
    -- times", which rule 603.12's other printed form ("when [something happens]
    -- this way") reaches -- that event is no payment and can occur several times
    -- in one resolution, where this fires once (#2121).
    Reflexive
  | -- | CR 701.54d: "whenever the Ring tempts you" (Nazgul), against
    -- GameEvent.RingTempted. Fires on the temptation itself, so one whose
    -- CR 701.54a actions were all impossible fires it too.
    RingTemptsPlayer PlayerRelation.PlayerRelation
  | -- | CR 509.3d read by a bystander: "whenever [a creature] becomes blocked by
    -- a creature", the Filter over the ATTACKER and the blocker bound under
    -- Pawl.Engine.Binding.blockingCreature (CR 701.54c's three-temptation tier).
    --
    -- Rule 509.3d's remaining producer, an effect that causes a creature to
    -- block (General Jarkeld), reaches it through the same arm as the other two.
    PermanentBecomesBlockedBy (Filter.Filter Keyword.Keyword)
  | -- | CR 701.68d: "whenever a player blights"
    -- (data\/cards\/synthetic-blight-chronicler.json), against
    -- GameEvent.Blighted. Fires on the blight itself, so one that put no
    -- counters -- rule 701.68a's N of zero, or a replacement that kept them off
    -- -- fires it too.
    PlayerBlights PlayerRelation.PlayerRelation
  | -- | CR 701.61a: "whenever you forage" (Corpseberry Cultivator), against
    -- GameEvent.Foraged. Fires on the forage itself, so which half of rule
    -- 701.61a the forager took does not separate two forages here.
    PlayerForages PlayerRelation.PlayerRelation
  | -- | CR 701.66b: "whenever a player earthbends"
    -- (data\/cards\/synthetic-stonelistener-adept.json), against
    -- GameEvent.Earthbent. Rule 701.66b puts the moment at the CREATION of rule
    -- 701.66a's delayed triggered ability, so this fires while the earthbend is
    -- still resolving and NOT when the land later dies and comes back.
    --
    -- One arm per bending keyword action, PlayerBlights' and PlayerForages'
    -- shape, rather than one arm carrying which act was done: CR 701.65b, CR
    -- 701.66b, CR 701.67c and CR 702.189b put the moment in four different
    -- places, and a printing watching several of them (Avatar Aang) writes CR
    -- 603.1b's AnyOf over these arms.
    PlayerEarthbends PlayerRelation.PlayerRelation
  | -- | CR 701.67c: "whenever a player waterbends"
    -- (data\/cards\/synthetic-tidecaller-scribe.json), against
    -- GameEvent.Waterbent. Fires on the PAYMENT, "regardless of how they paid
    -- that cost", so a waterbend cost paid entirely in mana fires it exactly as
    -- one paid by rule 701.67a's taps does.
    PlayerWaterbends PlayerRelation.PlayerRelation
  | -- | CR 701.65b: "whenever you airbend" (Avatar Aang), against
    -- GameEvent.Airbent.
    PlayerAirbends PlayerRelation.PlayerRelation
  | -- | CR 702.189b: "whenever you firebend" (Avatar Aang), against
    -- GameEvent.Firebent -- a firebending ability resolving, not the attack
    -- that triggered it.
    PlayerFirebends PlayerRelation.PlayerRelation
  deriving (Eq, Ord, Show)
