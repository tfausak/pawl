module Pawl.Codec.TriggerConditionSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TurnScope as TurnScope

-- | At least one case per TriggerCondition constructor. RoomFullyUnlocked gets
-- two, one per PlayerRelation, since CR 109.5's "you" against "an opponent" is
-- the whole of that payload.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggerCondition" $ do
  -- CR 603.6a.
  Spec.it s "SelfEnters" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfEnters
      " {\"type\":\"SelfEnters\"} "
  -- CR 603.6a's "[type]" is a whole Filter, so a nested And/Not has to survive
  -- the trip.
  Spec.it s "PermanentEnters round-trips with its Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentEnters (Filter.And [Filter.HasCardType CardType.Creature, Filter.Not Filter.IsSource]))
      " {\"type\":\"PermanentEnters\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}} "
  -- CR 603.2b.
  Spec.it s "StepBegins round-trips" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn))
      " {\"type\":\"StepBegins\",\"value\":{\"phase\":{\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}},\"scope\":{\"type\":\"EachTurn\"}}} "
  -- CR 603.8: a STATE trigger, carrying its Condition.
  Spec.it s "StateIs round-trips" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.StateIs (Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0))))
      " {\"type\":\"StateIs\",\"value\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  -- CR 603.2 / 509-510: the bearer dealt combat damage to a player.
  Spec.it s "SelfDealsCombatDamageToPlayer" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfDealsCombatDamageToPlayer
      " {\"type\":\"SelfDealsCombatDamageToPlayer\"} "
  -- CR 120.3: the same history read the other way round -- the bearer was DEALT
  -- damage. Nullary, since enrage qualifies the damage in no way.
  Spec.it s "SelfIsDealtDamage" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfIsDealtDamage
      " {\"type\":\"SelfIsDealtDamage\"} "
  -- The same event read by a bystander, carrying Tovolar's "you control": the
  -- Filter is the whole payload, so it has to survive both directions.
  Spec.it s "PermanentDealsCombatDamageToPlayer round-trips with its Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.ControlledBy PlayerRelation.You))
      " {\"type\":\"PermanentDealsCombatDamageToPlayer\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}} "
  -- CR 725.2: a creature dealt combat damage to the monarch.
  Spec.it s "CreatureDealtCombatDamageToMonarch" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.CreatureDealtCombatDamageToMonarch
      " {\"type\":\"CreatureDealtCombatDamageToMonarch\"} "
  -- CR 702.179d: one or more opponents lost life during your turn.
  Spec.it s "OpponentLostLifeDuringYourTurn" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.OpponentLostLifeDuringYourTurn
      " {\"type\":\"OpponentLostLifeDuringYourTurn\"} "
  -- CR 702.29c.
  Spec.it s "SelfCycled" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfCycled
      " {\"type\":\"SelfCycled\"} "
  -- CR 702.94a's linked half.
  Spec.it s "SelfRevealedForMiracle" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfRevealedForMiracle
      " {\"type\":\"SelfRevealedForMiracle\"} "
  -- CR 701.9a's discard, self-scoped: Bartered Cow's "when you discard this
  -- card". Nullary, SelfCycled's shape -- the ability is printed on the card that
  -- is discarded, so there is nothing left to say.
  Spec.it s "SelfDiscarded" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfDiscarded
      " {\"type\":\"SelfDiscarded\"} "
  -- CR 701.9a's discard. Both relations, since the PlayerRelation is the whole
  -- content of the "whenever an opponent discards" phrasing.
  Spec.it s "PlayerDiscards round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerDiscards PlayerRelation.Opponent)
      " {\"type\":\"PlayerDiscards\",\"value\":{\"type\":\"Opponent\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerDiscards PlayerRelation.You)
      " {\"type\":\"PlayerDiscards\",\"value\":{\"type\":\"You\"}} "
  -- CR 121.1's Nth draw of a turn. The relation and the ordinal are both content,
  -- and the ordinal is not 1, so a codec dropping it would fail rather than
  -- round-trip the default.
  Spec.it s "PlayerDrawsNthCard" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerDrawsNthCard (PlayerDrawsNthCard.MkPlayerDrawsNthCard PlayerRelation.You 2))
      " {\"type\":\"PlayerDrawsNthCard\",\"value\":{\"player\":{\"type\":\"You\"},\"nth\":2}} "
  -- CR 508.3a. Both frequencies, since "for the first time each turn" is a
  -- payload on this condition rather than a sibling one.
  Spec.it s "SelfAttacks round-trips both frequencies" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime)
      " {\"type\":\"SelfAttacks\",\"value\":{\"type\":\"EveryTime\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn)
      " {\"type\":\"SelfAttacks\",\"value\":{\"type\":\"FirstTimeEachTurn\"}} "
  -- CR 702.149a. A Filter over the OTHER attackers, where the sibling above takes
  -- a frequency and the one below counts the declaration.
  Spec.it s "SelfAttacksWithAnother" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfAttacksWithAnother Filter.PowerGreaterThanSource)
      " {\"type\":\"SelfAttacksWithAnother\",\"value\":{\"type\":\"PowerGreaterThanSource\"}} "
  -- CR 506.5. A Filter over the ATTACKER where the sibling above takes a
  -- frequency: "alone" is the constructor's own, so it has no encoding of its
  -- own here.
  Spec.it s "CreatureAttacksAlone" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.CreatureAttacksAlone (Filter.ControlledBy PlayerRelation.You))
      " {\"type\":\"CreatureAttacksAlone\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}} "
  -- CR 508.3a's second sentence, nullary where the sibling above takes a Filter:
  -- CR 508.1a already makes every declared attacker a creature.
  Spec.it s "CreatureAttacksYou" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.CreatureAttacksYou
      " {\"type\":\"CreatureAttacksYou\"} "
  -- CR 702.105a, nullary: the comparison is over life totals, so there is nothing
  -- for a card to parameterize.
  Spec.it s "SelfAttacksPlayerWithMostLife" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfAttacksPlayerWithMostLife
      " {\"type\":\"SelfAttacksPlayerWithMostLife\"} "
  -- CR 509.3a, and nullary where its mirror is not: rule 509.3a's
  -- once-each-combat is not a frequency a card chooses.
  Spec.it s "SelfBlocks" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfBlocks
      " {\"type\":\"SelfBlocks\"} "
  -- CR 509.3b, nullary too: the attacker it names is a BINDING rather than a
  -- payload on the condition, so a distinct tag is the whole encoding.
  Spec.it s "SelfBlocksCreature" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfBlocksCreature
      " {\"type\":\"SelfBlocksCreature\"} "
  -- CR 509.3e, and NOT nullary: the floor is the card's own number.
  Spec.it s "SelfBlocksAtLeast" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfBlocksAtLeast 2)
      " {\"type\":\"SelfBlocksAtLeast\",\"value\":2} "
  -- CR 509.3e's filtered form, whose tag must stay distinct from the counted one
  -- above: the two read the same grouped event and differ only in what they ask
  -- of it.
  Spec.it s "SelfBlocksOneOrMore" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfBlocksOneOrMore (Filter.HasColor Color.Black))
      " {\"type\":\"SelfBlocksOneOrMore\",\"value\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Black\"}}} "
  -- CR 509.3c, nullary for the same reason, and a distinct tag from the sibling
  -- above: the two name opposite sides of one declaration.
  Spec.it s "SelfBecomesBlocked" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfBecomesBlocked
      " {\"type\":\"SelfBecomesBlocked\"} "
  -- CR 509.3d, which carries a Filter over the BLOCKER where its once-each-combat
  -- sibling above carries nothing -- rule 702.25a's "without flanking".
  Spec.it s "SelfBecomesBlockedBy" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfBecomesBlockedBy (Filter.Not (Filter.HasKeyword Keyword.Flanking)))
      " {\"type\":\"SelfBecomesBlockedBy\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flanking\"}}}} "
  -- CR 509.3e read from the attacking side. A distinct tag from the per-blocker
  -- form above, which carries the same payload and differs only in arity.
  Spec.it s "SelfBecomesBlockedByOneOrMore" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.HasColor Color.Black))
      " {\"type\":\"SelfBecomesBlockedByOneOrMore\",\"value\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Black\"}}} "
  -- CR 509.1h's unblocked branch, nullary like the blocked one: "attacks and
  -- isn't blocked" has no per-blocker reading to carry a Filter for.
  Spec.it s "SelfAttacksUnblocked" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfAttacksUnblocked
      " {\"type\":\"SelfAttacksUnblocked\"} "
  -- CR 113.6k's condition, which names a zone pair rather than the battlefield.
  Spec.it s "SelfPutIntoGraveyardFromLibrary" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfPutIntoGraveyardFromLibrary
      " {\"type\":\"SelfPutIntoGraveyardFromLibrary\"} "
  -- The same rule with no origin zone at all -- a separate tag from the one
  -- above, which it is a superset of: the two must never decode to each other.
  Spec.it s "SelfPutIntoGraveyardFromAnywhere" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfPutIntoGraveyardFromAnywhere
      " {\"type\":\"SelfPutIntoGraveyardFromAnywhere\"} "
  -- CR 603.6c's second written form, abbreviated by CR 700.4 to "dies".
  Spec.it s "SelfDies" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfDies
      " {\"type\":\"SelfDies\"} "
  -- The same written form read by a bystander, which carries a Filter where
  -- SelfDies above carries nothing -- so it is a separate tag, and "another"
  -- lives inside that Filter.
  Spec.it s "PermanentDies round-trips with its Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentDies (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You, Filter.Not Filter.IsSource]))
      " {\"type\":\"PermanentDies\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}} "
  -- CR 603.6c's first written form, a separate tag from SelfDies above: the two
  -- must never decode to each other.
  Spec.it s "SelfLeavesTheBattlefield" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfLeavesTheBattlefield
      " {\"type\":\"SelfLeavesTheBattlefield\"} "
  -- CR 702.55b/702.55c's exile-zone death watch. Nullary: the link it matches on
  -- is board state, so nothing about it rides the condition.
  Spec.it s "HauntedCreatureDies" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.HauntedCreatureDies
      " {\"type\":\"HauntedCreatureDies\"} "
  -- CR 701.6a's countering. Both relations, for the same reason
  -- PlayerDiscards has both.
  Spec.it s "SpellOrAbilityCounters round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.You)
      " {\"type\":\"SpellOrAbilityCounters\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.Opponent)
      " {\"type\":\"SpellOrAbilityCounters\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 615.13's prevention trigger. Both relations, for the same reason.
  Spec.it s "DamageToPlayerPrevented round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.DamageToPlayerPrevented PlayerRelation.You)
      " {\"type\":\"DamageToPlayerPrevented\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.DamageToPlayerPrevented PlayerRelation.Opponent)
      " {\"type\":\"DamageToPlayerPrevented\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 119.9's life-gain trigger. Both relations, for the same reason.
  Spec.it s "PlayerGainsLife round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerGainsLife PlayerRelation.You)
      " {\"type\":\"PlayerGainsLife\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerGainsLife PlayerRelation.Opponent)
      " {\"type\":\"PlayerGainsLife\",\"value\":{\"type\":\"Opponent\"}} "
  -- The life-LOSS trigger. A DIFFERENT tag from PlayerGainsLife above and the
  -- same payload shape, so the two must never decode to each other -- the same
  -- hazard GameEvent's LifeLost/LifeGained pair carries.
  Spec.it s "PlayerLosesLife round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerLosesLife PlayerRelation.You)
      " {\"type\":\"PlayerLosesLife\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerLosesLife PlayerRelation.Opponent)
      " {\"type\":\"PlayerLosesLife\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 714.2b. The payload is the counter KIND then the chapter number, in that
  -- order, since a Saga's chapters are told apart by the number alone.
  Spec.it s "SelfCountersReached round-trips its kind and its chapter number" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached CounterKind.Lore 3))
      " {\"type\":\"SelfCountersReached\",\"value\":{\"kind\":{\"type\":\"Lore\"},\"amount\":3}} "
  -- CR 310.12b. The payload is the counter kind alone: "the last" needs no number.
  Spec.it s "SelfLastCounterRemoved round-trips its kind" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfLastCounterRemoved CounterKind.Defense)
      " {\"type\":\"SelfLastCounterRemoved\",\"value\":{\"type\":\"Defense\"}} "
  -- CR 601.2i. A PAIR: a Filter over the SPELL, holding Young Pyromancer's two
  -- printed narrowings -- who cast it and what it was -- plus the TurnScope,
  -- which is the axis the Filter cannot carry. Young Pyromancer prints no turn,
  -- so EachTurn is its half.
  Spec.it s "SpellCast round-trips with its Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SpellCast (SpellCast.MkSpellCast (Filter.And [Filter.ControlledBy PlayerRelation.You, Filter.Or [Filter.HasCardType CardType.Instant, Filter.HasCardType CardType.Sorcery]]) TurnScope.EachTurn Nothing Nothing))
      " {\"type\":\"SpellCast\",\"value\":{\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}},{\"type\":\"Or\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Instant\"}},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Sorcery\"}}]}]},\"scope\":{\"type\":\"EachTurn\"}}} "
  -- The other half moved on its own, over the plainest Filter there is: Brineborn
  -- Cutthroat's "during an opponent's turn", which no Filter could have said.
  Spec.it s "SpellCast round-trips an opponent's-turn scope" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SpellCast (SpellCast.MkSpellCast (Filter.ControlledBy PlayerRelation.You) TurnScope.OpponentsTurn Nothing Nothing))
      " {\"type\":\"SpellCast\",\"value\":{\"filter\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}},\"scope\":{\"type\":\"OpponentsTurn\"}}} "
  -- CR 601.2i read off the spell itself, Desolation Twin's "when you cast this
  -- spell": nullary, since "this spell" needs neither Filter nor TurnScope.
  -- CR 702.21a's relation is on the TARGETING side -- the opponent whose spell
  -- named the bearer -- so the payload is the same PlayerRelation
  -- SpellOrAbilityCounters carries and never a Filter over the bearer.
  Spec.it s "SelfBecomesTargeted" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfBecomesTargeted PlayerRelation.Opponent)
      " {\"type\":\"SelfBecomesTargeted\",\"value\":{\"type\":\"Opponent\"}} "
  Spec.it s "SelfCast" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfCast
      " {\"type\":\"SelfCast\"} "
  -- CR 709.5h. The payload is the DOOR's own name (CR 709.4a), which is what
  -- separates two unlock triggers printed on one Room.
  Spec.it s "SelfHalfUnlocked round-trips its door" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SelfHalfUnlocked (CardName.MkCardName (Text.pack "Steaming Sauna")))
      " {\"type\":\"SelfHalfUnlocked\",\"value\":\"Steaming Sauna\"} "
  -- CR 709.5i. The payload is a PlayerRelation, read against the controller of the
  -- permanent that became fully unlocked. BOTH relations, since the two are what
  -- separate "you fully unlock" from an opponent doing it.
  Spec.it s "RoomFullyUnlocked round-trips its relation" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.RoomFullyUnlocked PlayerRelation.You)
      " {\"type\":\"RoomFullyUnlocked\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "RoomFullyUnlocked round-trips the opponent relation too" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.RoomFullyUnlocked PlayerRelation.Opponent)
      " {\"type\":\"RoomFullyUnlocked\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 603.1b's "more than one trigger condition": one ability, two clauses. The
  -- only RECURSIVE condition, so both directions of the codec call themselves.
  --
  -- Balemurk Leech's own pair, which is neither empty nor a singleton on purpose:
  -- a codec that kept only the first branch, or that dropped the list entirely,
  -- would round-trip a one-element list unchanged and pass.
  Spec.it s "AnyOf round-trips both of its branches" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.AnyOf [TriggerCondition.PermanentEnters (Filter.HasCardType CardType.Enchantment), TriggerCondition.RoomFullyUnlocked PlayerRelation.You])
      " {\"type\":\"AnyOf\",\"value\":[{\"type\":\"PermanentEnters\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Enchantment\"}}},{\"type\":\"RoomFullyUnlocked\",\"value\":{\"type\":\"You\"}}]} "
  -- CR 708.7. Nullary: the bearer is CR 113.7a's source and the rule leaves
  -- nothing else for the condition to name.
  Spec.it s "SelfTurnedFaceUp" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfTurnedFaceUp
      " {\"type\":\"SelfTurnedFaceUp\"} "
  -- CR 708.7's other written form. Aven Farseer says only "a permanent", which is
  -- Filter's trivial `And []` -- so the encoded value carries an EMPTY filter
  -- rather than no filter at all, which is what keeps the narrowed printings
  -- below spellable in the same constructor.
  Spec.it s "PermanentTurnedFaceUp round-trips with the trivial Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentTurnedFaceUp (Filter.And []))
      " {\"type\":\"PermanentTurnedFaceUp\",\"value\":{\"type\":\"And\",\"value\":[]}} "
  -- The same condition NARROWED, which is Deathmist Raptor's "whenever a permanent
  -- you control is turned face up": the Filter is the whole difference between the
  -- two printings, so it has to survive both directions.
  Spec.it s "PermanentTurnedFaceUp round-trips with a narrowing Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentTurnedFaceUp (Filter.ControlledBy PlayerRelation.You))
      " {\"type\":\"PermanentTurnedFaceUp\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}} "
  -- CR 702.112b's designation, carrying Valeron Wardens' own narrowing: the pair of
  -- designation and Filter is the whole payload, so both have to survive both
  -- directions -- a dropped designation would make this condition match Arbor
  -- Colossus' monstrous event too.
  Spec.it s "PermanentBecomesDesignated round-trips with its designation and Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated Designation.Renowned (Filter.ControlledBy PlayerRelation.You)))
      " {\"type\":\"PermanentBecomesDesignated\",\"value\":{\"designation\":{\"type\":\"Renowned\"},\"filter\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}} "
  -- CR 701.37b through the same constructor: Arbor Colossus' "when this creature
  -- becomes monstrous", which is Filter.IsSource beside the other designation.
  Spec.it s "PermanentBecomesDesignated carries Monstrous" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated Designation.Monstrous Filter.IsSource))
      " {\"type\":\"PermanentBecomesDesignated\",\"value\":{\"designation\":{\"type\":\"Monstrous\"},\"filter\":{\"type\":\"IsSource\"}}} "
  -- CR 702.100b's marker, self-scoped, so nullary.
  Spec.it s "SelfEvolves" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfEvolves
      " {\"type\":\"SelfEvolves\"} "
  -- CR 702.134c's marker, read through the source's attachment, so nullary for
  -- SelfEvolves' reason: neither the mentor nor the mentored creature is named by
  -- the condition.
  Spec.it s "AttachedCreatureMentors" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.AttachedCreatureMentors
      " {\"type\":\"AttachedCreatureMentors\"} "
  -- CR 702.149c's marker, self-scoped, so nullary for SelfEvolves' reason.
  Spec.it s "SelfTrains" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfTrains
      " {\"type\":\"SelfTrains\"} "
  -- CR 603.10a's sacrifice family. Nullary: "a player" is neither PlayerRelation
  -- arm and "a permanent" names no Filter, so there is nothing to encode.
  Spec.it s "PermanentSacrificed" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.PermanentSacrificed
      " {\"type\":\"PermanentSacrificed\"} "
  -- CR 603.3b's second class. The relation is the only payload: which Saga and
  -- which chapter are read off the event and the source's projection.
  Spec.it s "SagaFinalChapterTriggers round-trips its relation" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SagaFinalChapterTriggers PlayerRelation.You)
      " {\"type\":\"SagaFinalChapterTriggers\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "SagaFinalChapterTriggers round-trips the opponent relation too" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.SagaFinalChapterTriggers PlayerRelation.Opponent)
      " {\"type\":\"SagaFinalChapterTriggers\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 725.1's crowning. Both relations, for the same reason PlayerDiscards has
  -- both: the PlayerRelation is the whole difference between "whenever you
  -- become the monarch" and "whenever an opponent becomes the monarch", so a
  -- codec that dropped it would silently turn one printing into the other.
  Spec.it s "PlayerBecomesMonarch round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerBecomesMonarch PlayerRelation.You)
      " {\"type\":\"PlayerBecomesMonarch\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerBecomesMonarch PlayerRelation.Opponent)
      " {\"type\":\"PlayerBecomesMonarch\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 603.7's slot-named condition. The slot is a bare string, as every other
  -- SlotName in card data is, and it is the whole payload: a codec that dropped it
  -- would leave the condition asking about no creature at all.
  Spec.it s "LoseControlOfBound round-trips its slot" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.LoseControlOfBound (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"LoseControlOfBound\",\"value\":\"target\"} "
  -- CR 309.4c. No dungeon card prints this condition -- Pawl.Engine.Dungeon mints
  -- one per room -- but it round-trips like every other arm.
  Spec.it s "RoomEntered round-trips its room" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.RoomEntered (RoomIndex.MkRoomIndex 3))
      " {\"type\":\"RoomEntered\",\"value\":3} "
  -- CR 701.22d and CR 701.25d. Two TAGS rather than one carrying which keyword
  -- action it was: Matoya, Archon Elder's "whenever you scry or surveil" is an
  -- AnyOf of the two, and a codec that folded them would fire the card twice on
  -- one scry. Both relations, for PlayerBecomesMonarch's reason.
  Spec.it s "PlayerScries round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerScries PlayerRelation.You)
      " {\"type\":\"PlayerScries\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerScries PlayerRelation.Opponent)
      " {\"type\":\"PlayerScries\",\"value\":{\"type\":\"Opponent\"}} "
  Spec.it s "PlayerSurveils round-trips both relations" $ do
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerSurveils PlayerRelation.You)
      " {\"type\":\"PlayerSurveils\",\"value\":{\"type\":\"You\"}} "
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlayerSurveils PlayerRelation.Opponent)
      " {\"type\":\"PlayerSurveils\",\"value\":{\"type\":\"Opponent\"}} "
  -- CR 702.170e. Nullary, SelfCycled's shape: the ability is printed on the card
  -- that becomes plotted, so there is nothing to select among.
  Spec.it s "SelfBecomesPlotted round-trips" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfBecomesPlotted
      " {\"type\":\"SelfBecomesPlotted\"} "
  -- CR 701.44b. The Filter is Wildgrowth Walker's "a creature you control" and
  -- describes the EXPLORER, so a codec that dropped it would grow the Walker off
  -- an opponent's explore.
  Spec.it s "PermanentExplores round-trips with its Filter" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PermanentExplores (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You]))
      " {\"type\":\"PermanentExplores\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}]}} "
  -- CR 701.43d. Nullary, SelfEvolves' shape: rule 701.43d links the trigger to
  -- the static ability printed beside it, so its subject is always the bearer.
  Spec.it s "SelfExerted round-trips" $
    Common.assertCodec
      s
      TriggerCondition.codec
      TriggerCondition.SelfExerted
      " {\"type\":\"SelfExerted\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TriggerCondition.codec
