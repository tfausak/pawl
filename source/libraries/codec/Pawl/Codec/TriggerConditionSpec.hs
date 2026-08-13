{-# LANGUAGE MultilineStrings #-}

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
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SlotName as SlotName
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
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfEnters
      """ {"type":"SelfEnters"} """
  -- CR 603.6a's "[type]" is a whole Filter, so a nested And/Not has to survive
  -- the trip.
  Spec.it s "PermanentEnters round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentEnters (Filter.And [Filter.HasCardType CardType.Creature, Filter.Not Filter.IsSource]))
      """ {"type":"PermanentEnters","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"Not","value":{"type":"IsSource"}}]}} """
  -- CR 603.2b.
  Spec.it s "StepBegins round-trips" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn)
      """ {"type":"StepBegins","value":[{"type":"Ending","value":{"type":"EndStep"}},{"type":"EachTurn"}]} """
  -- CR 603.8: a STATE trigger, carrying its Condition.
  Spec.it s "StateIs round-trips" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.StateIs (Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0))))
      """ {"type":"StateIs","value":{"type":"Compares","value":{"measured":{"type":"Literal","value":0},"comparison":{"type":"Exactly"},"threshold":{"type":"Literal","value":0}}}} """
  -- CR 603.2 / 509-510: the bearer dealt combat damage to a player.
  Spec.it s "SelfDealsCombatDamageToPlayer" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfDealsCombatDamageToPlayer
      """ {"type":"SelfDealsCombatDamageToPlayer"} """
  -- The same event read by a bystander, carrying Tovolar's "you control": the
  -- Filter is the whole payload, so it has to survive both directions.
  Spec.it s "PermanentDealsCombatDamageToPlayer round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.ControlledBy PlayerRelation.You))
      """ {"type":"PermanentDealsCombatDamageToPlayer","value":{"type":"ControlledBy","value":{"type":"You"}}} """
  -- CR 725.2: a creature dealt combat damage to the monarch.
  Spec.it s "CreatureDealtCombatDamageToMonarch" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.CreatureDealtCombatDamageToMonarch
      """ {"type":"CreatureDealtCombatDamageToMonarch"} """
  -- CR 702.179d: one or more opponents lost life during your turn.
  Spec.it s "OpponentLostLifeDuringYourTurn" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.OpponentLostLifeDuringYourTurn
      """ {"type":"OpponentLostLifeDuringYourTurn"} """
  -- CR 702.29c.
  Spec.it s "SelfCycled" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfCycled
      """ {"type":"SelfCycled"} """
  -- CR 702.94a's linked half.
  Spec.it s "SelfRevealedForMiracle" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfRevealedForMiracle
      """ {"type":"SelfRevealedForMiracle"} """
  -- CR 701.9a's discard. Both relations, since the PlayerRelation is the whole
  -- content of the "whenever an opponent discards" phrasing.
  Spec.it s "PlayerDiscards round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDiscards PlayerRelation.Opponent)
      """ {"type":"PlayerDiscards","value":{"type":"Opponent"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDiscards PlayerRelation.You)
      """ {"type":"PlayerDiscards","value":{"type":"You"}} """
  -- CR 121.1's Nth draw of a turn. The relation and the ordinal are both content,
  -- and the ordinal is not 1, so a codec dropping it would fail rather than
  -- round-trip the default.
  Spec.it s "PlayerDrawsNthCard" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDrawsNthCard PlayerRelation.You 2)
      """ {"type":"PlayerDrawsNthCard","value":[{"type":"You"},2]} """
  -- CR 508.3a. Both frequencies, since "for the first time each turn" is a
  -- payload on this condition rather than a sibling one.
  Spec.it s "SelfAttacks round-trips both frequencies" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime)
      """ {"type":"SelfAttacks","value":{"type":"EveryTime"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn)
      """ {"type":"SelfAttacks","value":{"type":"FirstTimeEachTurn"}} """
  -- CR 702.149a. A Filter over the OTHER attackers, where the sibling above takes
  -- a frequency and the one below counts the declaration.
  Spec.it s "SelfAttacksWithAnother" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacksWithAnother Filter.PowerGreaterThanSource)
      """ {"type":"SelfAttacksWithAnother","value":{"type":"PowerGreaterThanSource"}} """
  -- CR 506.5. A Filter over the ATTACKER where the sibling above takes a
  -- frequency: "alone" is the constructor's own, so it has no encoding of its
  -- own here.
  Spec.it s "CreatureAttacksAlone" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.CreatureAttacksAlone (Filter.ControlledBy PlayerRelation.You))
      """ {"type":"CreatureAttacksAlone","value":{"type":"ControlledBy","value":{"type":"You"}}} """
  -- CR 702.105a, nullary: the comparison is over life totals, so there is nothing
  -- for a card to parameterize.
  Spec.it s "SelfAttacksPlayerWithMostLife" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfAttacksPlayerWithMostLife
      """ {"type":"SelfAttacksPlayerWithMostLife"} """
  -- CR 509.3a, and nullary where its mirror is not: rule 509.3a's
  -- once-each-combat is not a frequency a card chooses.
  Spec.it s "SelfBlocks" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfBlocks
      """ {"type":"SelfBlocks"} """
  -- CR 509.3b, nullary too: the attacker it names is a BINDING rather than a
  -- payload on the condition, so a distinct tag is the whole encoding.
  Spec.it s "SelfBlocksCreature" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfBlocksCreature
      """ {"type":"SelfBlocksCreature"} """
  -- CR 509.3e, and NOT nullary: the floor is the card's own number.
  Spec.it s "SelfBlocksAtLeast" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfBlocksAtLeast 2)
      """ {"type":"SelfBlocksAtLeast","value":2} """
  -- CR 509.3e's filtered form, whose tag must stay distinct from the counted one
  -- above: the two read the same grouped event and differ only in what they ask
  -- of it.
  Spec.it s "SelfBlocksOneOrMore" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfBlocksOneOrMore (Filter.HasColor Color.Black))
      """ {"type":"SelfBlocksOneOrMore","value":{"type":"HasColor","value":{"type":"Black"}}} """
  -- CR 509.3c, nullary for the same reason, and a distinct tag from the sibling
  -- above: the two name opposite sides of one declaration.
  Spec.it s "SelfBecomesBlocked" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfBecomesBlocked
      """ {"type":"SelfBecomesBlocked"} """
  -- CR 509.3d, which carries a Filter over the BLOCKER where its once-each-combat
  -- sibling above carries nothing -- rule 702.25a's "without flanking".
  Spec.it s "SelfBecomesBlockedBy" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfBecomesBlockedBy (Filter.Not (Filter.HasKeyword Keyword.Flanking)))
      """ {"type":"SelfBecomesBlockedBy","value":{"type":"Not","value":{"type":"HasKeyword","value":{"type":"Flanking"}}}} """
  -- CR 509.3e read from the attacking side. A distinct tag from the per-blocker
  -- form above, which carries the same payload and differs only in arity.
  Spec.it s "SelfBecomesBlockedByOneOrMore" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.HasColor Color.Black))
      """ {"type":"SelfBecomesBlockedByOneOrMore","value":{"type":"HasColor","value":{"type":"Black"}}} """
  -- CR 113.6k's condition, which names a zone pair rather than the battlefield.
  Spec.it s "SelfPutIntoGraveyardFromLibrary" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfPutIntoGraveyardFromLibrary
      """ {"type":"SelfPutIntoGraveyardFromLibrary"} """
  -- The same rule with no origin zone at all -- a separate tag from the one
  -- above, which it is a superset of: the two must never decode to each other.
  Spec.it s "SelfPutIntoGraveyardFromAnywhere" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfPutIntoGraveyardFromAnywhere
      """ {"type":"SelfPutIntoGraveyardFromAnywhere"} """
  -- CR 603.6c's second written form, abbreviated by CR 700.4 to "dies".
  Spec.it s "SelfDies" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfDies
      """ {"type":"SelfDies"} """
  -- The same written form read by a bystander, which carries a Filter where
  -- SelfDies above carries nothing -- so it is a separate tag, and "another"
  -- lives inside that Filter.
  Spec.it s "PermanentDies round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentDies (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You, Filter.Not Filter.IsSource]))
      """ {"type":"PermanentDies","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"ControlledBy","value":{"type":"You"}},{"type":"Not","value":{"type":"IsSource"}}]}} """
  -- CR 603.6c's first written form, a separate tag from SelfDies above: the two
  -- must never decode to each other.
  Spec.it s "SelfLeavesTheBattlefield" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfLeavesTheBattlefield
      """ {"type":"SelfLeavesTheBattlefield"} """
  -- CR 702.55b/702.55c's exile-zone death watch. Nullary: the link it matches on
  -- is board state, so nothing about it rides the condition.
  Spec.it s "HauntedCreatureDies" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.HauntedCreatureDies
      """ {"type":"HauntedCreatureDies"} """
  -- CR 701.6a's countering. Both relations, for the same reason
  -- PlayerDiscards has both.
  Spec.it s "SpellOrAbilityCounters round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.You)
      """ {"type":"SpellOrAbilityCounters","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.Opponent)
      """ {"type":"SpellOrAbilityCounters","value":{"type":"Opponent"}} """
  -- CR 615.13's prevention trigger. Both relations, for the same reason.
  Spec.it s "DamageToPlayerPrevented round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.DamageToPlayerPrevented PlayerRelation.You)
      """ {"type":"DamageToPlayerPrevented","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.DamageToPlayerPrevented PlayerRelation.Opponent)
      """ {"type":"DamageToPlayerPrevented","value":{"type":"Opponent"}} """
  -- CR 119.9's life-gain trigger. Both relations, for the same reason.
  Spec.it s "PlayerGainsLife round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerGainsLife PlayerRelation.You)
      """ {"type":"PlayerGainsLife","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerGainsLife PlayerRelation.Opponent)
      """ {"type":"PlayerGainsLife","value":{"type":"Opponent"}} """
  -- The life-LOSS trigger. A DIFFERENT tag from PlayerGainsLife above and the
  -- same payload shape, so the two must never decode to each other -- the same
  -- hazard GameEvent's LifeLost/LifeGained pair carries.
  Spec.it s "PlayerLosesLife round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerLosesLife PlayerRelation.You)
      """ {"type":"PlayerLosesLife","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerLosesLife PlayerRelation.Opponent)
      """ {"type":"PlayerLosesLife","value":{"type":"Opponent"}} """
  -- CR 714.2b. The payload is the counter KIND then the chapter number, in that
  -- order, since a Saga's chapters are told apart by the number alone.
  Spec.it s "SelfCountersReached round-trips its kind and its chapter number" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfCountersReached CounterKind.Lore 3)
      """ {"type":"SelfCountersReached","value":[{"type":"Lore"},3]} """
  -- CR 310.11b. The payload is the counter kind alone: "the last" needs no number.
  Spec.it s "SelfLastCounterRemoved round-trips its kind" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfLastCounterRemoved CounterKind.Defense)
      """ {"type":"SelfLastCounterRemoved","value":{"type":"Defense"}} """
  -- CR 601.2i. A PAIR: a Filter over the SPELL, holding Young Pyromancer's two
  -- printed narrowings -- who cast it and what it was -- plus the TurnScope,
  -- which is the axis the Filter cannot carry. Young Pyromancer prints no turn,
  -- so EachTurn is its half.
  Spec.it s "SpellCast round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellCast (Filter.And [Filter.ControlledBy PlayerRelation.You, Filter.Or [Filter.HasCardType CardType.Instant, Filter.HasCardType CardType.Sorcery]]) TurnScope.EachTurn)
      """ {"type":"SpellCast","value":[{"type":"And","value":[{"type":"ControlledBy","value":{"type":"You"}},{"type":"Or","value":[{"type":"HasCardType","value":{"type":"Instant"}},{"type":"HasCardType","value":{"type":"Sorcery"}}]}]},{"type":"EachTurn"}]} """
  -- The other half moved on its own, over the plainest Filter there is: Brineborn
  -- Cutthroat's "during an opponent's turn", which no Filter could have said.
  Spec.it s "SpellCast round-trips an opponent's-turn scope" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellCast (Filter.ControlledBy PlayerRelation.You) TurnScope.OpponentsTurn)
      """ {"type":"SpellCast","value":[{"type":"ControlledBy","value":{"type":"You"}},{"type":"OpponentsTurn"}]} """
  -- CR 601.2i read off the spell itself, Desolation Twin's "when you cast this
  -- spell": nullary, since "this spell" needs neither Filter nor TurnScope.
  Spec.it s "SelfCast" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfCast
      """ {"type":"SelfCast"} """
  -- CR 709.5h. The payload is the DOOR's own name (CR 709.4a), which is what
  -- separates two unlock triggers printed on one Room.
  Spec.it s "SelfHalfUnlocked round-trips its door" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfHalfUnlocked (CardName.MkCardName (Text.pack "Steaming Sauna")))
      """ {"type":"SelfHalfUnlocked","value":"Steaming Sauna"} """
  -- CR 709.5i. The payload is a PlayerRelation, read against the controller of the
  -- permanent that became fully unlocked. BOTH relations, since the two are what
  -- separate "you fully unlock" from an opponent doing it.
  Spec.it s "RoomFullyUnlocked round-trips its relation" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.RoomFullyUnlocked PlayerRelation.You)
      """ {"type":"RoomFullyUnlocked","value":{"type":"You"}} """
  Spec.it s "RoomFullyUnlocked round-trips the opponent relation too" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.RoomFullyUnlocked PlayerRelation.Opponent)
      """ {"type":"RoomFullyUnlocked","value":{"type":"Opponent"}} """
  -- CR 603.1b's "more than one trigger condition": one ability, two clauses. The
  -- only RECURSIVE condition, so both directions of the codec call themselves.
  --
  -- Balemurk Leech's own pair, which is neither empty nor a singleton on purpose:
  -- a codec that kept only the first branch, or that dropped the list entirely,
  -- would round-trip a one-element list unchanged and pass.
  Spec.it s "AnyOf round-trips both of its branches" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.AnyOf [TriggerCondition.PermanentEnters (Filter.HasCardType CardType.Enchantment), TriggerCondition.RoomFullyUnlocked PlayerRelation.You])
      """ {"type":"AnyOf","value":[{"type":"PermanentEnters","value":{"type":"HasCardType","value":{"type":"Enchantment"}}},{"type":"RoomFullyUnlocked","value":{"type":"You"}}]} """
  -- CR 708.7. Nullary: the bearer is CR 113.7a's source and the rule leaves
  -- nothing else for the condition to name.
  Spec.it s "SelfTurnedFaceUp" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfTurnedFaceUp
      """ {"type":"SelfTurnedFaceUp"} """
  -- CR 708.7's other written form. Aven Farseer says only "a permanent", which is
  -- Filter's trivial `And []` -- so the encoded value carries an EMPTY filter
  -- rather than no filter at all, which is what keeps the narrowed printings
  -- below spellable in the same constructor.
  Spec.it s "PermanentTurnedFaceUp round-trips with the trivial Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentTurnedFaceUp (Filter.And []))
      """ {"type":"PermanentTurnedFaceUp","value":{"type":"And","value":[]}} """
  -- The same condition NARROWED, which is Deathmist Raptor's "whenever a permanent
  -- you control is turned face up": the Filter is the whole difference between the
  -- two printings, so it has to survive both directions.
  Spec.it s "PermanentTurnedFaceUp round-trips with a narrowing Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentTurnedFaceUp (Filter.ControlledBy PlayerRelation.You))
      """ {"type":"PermanentTurnedFaceUp","value":{"type":"ControlledBy","value":{"type":"You"}}} """
  -- CR 702.112b's designation, carrying Valeron Wardens' own narrowing: the pair of
  -- designation and Filter is the whole payload, so both have to survive both
  -- directions -- a dropped designation would make this condition match Arbor
  -- Colossus' monstrous event too.
  Spec.it s "PermanentBecomesDesignated round-trips with its designation and Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentBecomesDesignated Designation.Renowned (Filter.ControlledBy PlayerRelation.You))
      """ {"type":"PermanentBecomesDesignated","value":[{"type":"Renowned"},{"type":"ControlledBy","value":{"type":"You"}}]} """
  -- CR 701.37b through the same constructor: Arbor Colossus' "when this creature
  -- becomes monstrous", which is Filter.IsSource beside the other designation.
  Spec.it s "PermanentBecomesDesignated carries Monstrous" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentBecomesDesignated Designation.Monstrous Filter.IsSource)
      """ {"type":"PermanentBecomesDesignated","value":[{"type":"Monstrous"},{"type":"IsSource"}]} """
  -- CR 702.100b's marker, self-scoped, so nullary.
  Spec.it s "SelfEvolves" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfEvolves
      """ {"type":"SelfEvolves"} """
  -- CR 702.134c's marker, read through the source's attachment, so nullary for
  -- SelfEvolves' reason: neither the mentor nor the mentored creature is named by
  -- the condition.
  Spec.it s "AttachedCreatureMentors" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.AttachedCreatureMentors
      """ {"type":"AttachedCreatureMentors"} """
  -- CR 603.10a's sacrifice family. Nullary: "a player" is neither PlayerRelation
  -- arm and "a permanent" names no Filter, so there is nothing to encode.
  Spec.it s "PermanentSacrificed" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.PermanentSacrificed
      """ {"type":"PermanentSacrificed"} """
  -- CR 603.3b's second class. The relation is the only payload: which Saga and
  -- which chapter are read off the event and the source's projection.
  Spec.it s "SagaFinalChapterTriggers round-trips its relation" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SagaFinalChapterTriggers PlayerRelation.You)
      """ {"type":"SagaFinalChapterTriggers","value":{"type":"You"}} """
  Spec.it s "SagaFinalChapterTriggers round-trips the opponent relation too" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SagaFinalChapterTriggers PlayerRelation.Opponent)
      """ {"type":"SagaFinalChapterTriggers","value":{"type":"Opponent"}} """
  -- CR 725.1's crowning. Both relations, for the same reason PlayerDiscards has
  -- both: the PlayerRelation is the whole difference between "whenever you
  -- become the monarch" and "whenever an opponent becomes the monarch", so a
  -- codec that dropped it would silently turn one printing into the other.
  Spec.it s "PlayerBecomesMonarch round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerBecomesMonarch PlayerRelation.You)
      """ {"type":"PlayerBecomesMonarch","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerBecomesMonarch PlayerRelation.Opponent)
      """ {"type":"PlayerBecomesMonarch","value":{"type":"Opponent"}} """
  -- CR 603.7's slot-named condition. The slot is a bare string, as every other
  -- SlotName in card data is, and it is the whole payload: a codec that dropped it
  -- would leave the condition asking about no creature at all.
  Spec.it s "LoseControlOfBound round-trips its slot" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.LoseControlOfBound (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"LoseControlOfBound","value":"target"} """
  -- CR 309.4c. No dungeon card prints this condition -- Pawl.Engine.Dungeon mints
  -- one per room -- but it round-trips like every other arm.
  Spec.it s "RoomEntered round-trips its room" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.RoomEntered (RoomIndex.MkRoomIndex 3))
      """ {"type":"RoomEntered","value":3} """
