module Pawl.Codec.TriggerCondition where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Codec.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Codec.SelfCountersReached as SelfCountersReached
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.SpellCast as SpellCast
import qualified Pawl.Codec.StepBegins as StepBegins
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TriggerCondition as TriggerCondition

-- | RECURSIVE, through the one arm that holds conditions: 'AnyOf' names 'codec'
-- inside its own definition. That terminates because 'Arm.tagged' reaches WHNF
-- as a 'Codec.MkCodec' without forcing its arm list, and the SCHEMA terminates
-- because 'Define.define' registers this type's name before running the body,
-- so the re-entry emits a @$ref@ rather than recursing.
--
-- The five two-element-array payloads this codec used to write are now named
-- objects with records behind them (#1305).
codec :: Codec.Codec TriggerCondition.TriggerCondition
codec =
  Arm.tagged
    encode
    [ Arm.nullary "SelfEnters" TriggerCondition.SelfEnters,
      Arm.payload "PermanentEnters" filterCodec TriggerCondition.PermanentEnters,
      Arm.payload "StepBegins" StepBegins.codec TriggerCondition.StepBegins,
      Arm.payload "StateIs" Condition.codec TriggerCondition.StateIs,
      Arm.nullary "SelfDealsCombatDamageToPlayer" TriggerCondition.SelfDealsCombatDamageToPlayer,
      Arm.payload "PermanentDealsCombatDamageToPlayer" filterCodec TriggerCondition.PermanentDealsCombatDamageToPlayer,
      Arm.nullary "CreatureDealtCombatDamageToMonarch" TriggerCondition.CreatureDealtCombatDamageToMonarch,
      Arm.nullary "OpponentLostLifeDuringYourTurn" TriggerCondition.OpponentLostLifeDuringYourTurn,
      Arm.payload "SelfAttacks" TriggerFrequency.codec TriggerCondition.SelfAttacks,
      Arm.payload "SelfAttacksWithAnother" filterCodec TriggerCondition.SelfAttacksWithAnother,
      Arm.payload "CreatureAttacksAlone" filterCodec TriggerCondition.CreatureAttacksAlone,
      Arm.nullary "SelfAttacksPlayerWithMostLife" TriggerCondition.SelfAttacksPlayerWithMostLife,
      Arm.nullary "SelfBlocks" TriggerCondition.SelfBlocks,
      Arm.nullary "SelfBlocksCreature" TriggerCondition.SelfBlocksCreature,
      Arm.payload "SelfBlocksAtLeast" Common.natural TriggerCondition.SelfBlocksAtLeast,
      Arm.payload "SelfBlocksOneOrMore" filterCodec TriggerCondition.SelfBlocksOneOrMore,
      Arm.nullary "SelfBecomesBlocked" TriggerCondition.SelfBecomesBlocked,
      Arm.payload "SelfBecomesBlockedBy" filterCodec TriggerCondition.SelfBecomesBlockedBy,
      Arm.payload "SelfBecomesBlockedByOneOrMore" filterCodec TriggerCondition.SelfBecomesBlockedByOneOrMore,
      Arm.nullary "SelfCycled" TriggerCondition.SelfCycled,
      Arm.nullary "SelfRevealedForMiracle" TriggerCondition.SelfRevealedForMiracle,
      Arm.payload "PlayerDiscards" PlayerRelation.codec TriggerCondition.PlayerDiscards,
      Arm.payload "PlayerDrawsNthCard" PlayerDrawsNthCard.codec TriggerCondition.PlayerDrawsNthCard,
      Arm.nullary "SelfPutIntoGraveyardFromLibrary" TriggerCondition.SelfPutIntoGraveyardFromLibrary,
      Arm.nullary "SelfPutIntoGraveyardFromAnywhere" TriggerCondition.SelfPutIntoGraveyardFromAnywhere,
      Arm.nullary "SelfDies" TriggerCondition.SelfDies,
      Arm.payload "PermanentDies" filterCodec TriggerCondition.PermanentDies,
      Arm.nullary "SelfLeavesTheBattlefield" TriggerCondition.SelfLeavesTheBattlefield,
      Arm.nullary "HauntedCreatureDies" TriggerCondition.HauntedCreatureDies,
      Arm.payload "SpellOrAbilityCounters" PlayerRelation.codec TriggerCondition.SpellOrAbilityCounters,
      Arm.payload "DamageToPlayerPrevented" PlayerRelation.codec TriggerCondition.DamageToPlayerPrevented,
      Arm.payload "PlayerGainsLife" PlayerRelation.codec TriggerCondition.PlayerGainsLife,
      Arm.payload "PlayerLosesLife" PlayerRelation.codec TriggerCondition.PlayerLosesLife,
      Arm.payload "SelfCountersReached" SelfCountersReached.codec TriggerCondition.SelfCountersReached,
      Arm.payload "SelfLastCounterRemoved" counterKindCodec TriggerCondition.SelfLastCounterRemoved,
      Arm.payload "SpellCast" SpellCast.codec TriggerCondition.SpellCast,
      Arm.nullary "SelfCast" TriggerCondition.SelfCast,
      Arm.payload "SelfHalfUnlocked" CardName.codec TriggerCondition.SelfHalfUnlocked,
      Arm.payload "RoomFullyUnlocked" PlayerRelation.codec TriggerCondition.RoomFullyUnlocked,
      Arm.payload "AnyOf" (Common.list codec) TriggerCondition.AnyOf,
      Arm.nullary "SelfTurnedFaceUp" TriggerCondition.SelfTurnedFaceUp,
      Arm.payload "PermanentTurnedFaceUp" filterCodec TriggerCondition.PermanentTurnedFaceUp,
      Arm.payload "PermanentBecomesDesignated" PermanentBecomesDesignated.codec TriggerCondition.PermanentBecomesDesignated,
      Arm.nullary "SelfEvolves" TriggerCondition.SelfEvolves,
      Arm.nullary "AttachedCreatureMentors" TriggerCondition.AttachedCreatureMentors,
      Arm.nullary "PermanentSacrificed" TriggerCondition.PermanentSacrificed,
      Arm.payload "SagaFinalChapterTriggers" PlayerRelation.codec TriggerCondition.SagaFinalChapterTriggers,
      Arm.payload "PlayerBecomesMonarch" PlayerRelation.codec TriggerCondition.PlayerBecomesMonarch,
      Arm.payload "LoseControlOfBound" SlotName.codec TriggerCondition.LoseControlOfBound,
      Arm.payload "RoomEntered" RoomIndex.codec TriggerCondition.RoomEntered
    ]
  where
    filterCodec = Filter.codec Keyword.codec
    counterKindCodec = CounterKind.codec Keyword.codec
    tag t = Common.tagged t . Just
    encode c = case c of
      TriggerCondition.SelfEnters -> Common.nullary "SelfEnters"
      TriggerCondition.PermanentEnters f -> tag "PermanentEnters" $ Codec.encode filterCodec f
      TriggerCondition.StepBegins x -> tag "StepBegins" $ Codec.encode StepBegins.codec x
      TriggerCondition.StateIs c2 -> tag "StateIs" $ Codec.encode Condition.codec c2
      TriggerCondition.SelfDealsCombatDamageToPlayer -> Common.nullary "SelfDealsCombatDamageToPlayer"
      TriggerCondition.PermanentDealsCombatDamageToPlayer f -> tag "PermanentDealsCombatDamageToPlayer" $ Codec.encode filterCodec f
      TriggerCondition.CreatureDealtCombatDamageToMonarch -> Common.nullary "CreatureDealtCombatDamageToMonarch"
      TriggerCondition.OpponentLostLifeDuringYourTurn -> Common.nullary "OpponentLostLifeDuringYourTurn"
      TriggerCondition.SelfAttacks f -> tag "SelfAttacks" $ Codec.encode TriggerFrequency.codec f
      TriggerCondition.SelfAttacksWithAnother f -> tag "SelfAttacksWithAnother" $ Codec.encode filterCodec f
      TriggerCondition.CreatureAttacksAlone f -> tag "CreatureAttacksAlone" $ Codec.encode filterCodec f
      TriggerCondition.SelfAttacksPlayerWithMostLife -> Common.nullary "SelfAttacksPlayerWithMostLife"
      TriggerCondition.SelfBlocks -> Common.nullary "SelfBlocks"
      TriggerCondition.SelfBlocksCreature -> Common.nullary "SelfBlocksCreature"
      TriggerCondition.SelfBlocksAtLeast n -> tag "SelfBlocksAtLeast" $ Common.encodeNatural n
      TriggerCondition.SelfBlocksOneOrMore f -> tag "SelfBlocksOneOrMore" $ Codec.encode filterCodec f
      TriggerCondition.SelfBecomesBlocked -> Common.nullary "SelfBecomesBlocked"
      TriggerCondition.SelfBecomesBlockedBy f -> tag "SelfBecomesBlockedBy" $ Codec.encode filterCodec f
      TriggerCondition.SelfBecomesBlockedByOneOrMore f -> tag "SelfBecomesBlockedByOneOrMore" $ Codec.encode filterCodec f
      TriggerCondition.SelfCycled -> Common.nullary "SelfCycled"
      TriggerCondition.SelfRevealedForMiracle -> Common.nullary "SelfRevealedForMiracle"
      TriggerCondition.PlayerDiscards r -> tag "PlayerDiscards" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.PlayerDrawsNthCard x -> tag "PlayerDrawsNthCard" $ Codec.encode PlayerDrawsNthCard.codec x
      TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Common.nullary "SelfPutIntoGraveyardFromLibrary"
      TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Common.nullary "SelfPutIntoGraveyardFromAnywhere"
      TriggerCondition.SelfDies -> Common.nullary "SelfDies"
      TriggerCondition.PermanentDies f -> tag "PermanentDies" $ Codec.encode filterCodec f
      TriggerCondition.SelfLeavesTheBattlefield -> Common.nullary "SelfLeavesTheBattlefield"
      TriggerCondition.HauntedCreatureDies -> Common.nullary "HauntedCreatureDies"
      TriggerCondition.SpellOrAbilityCounters r -> tag "SpellOrAbilityCounters" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.DamageToPlayerPrevented r -> tag "DamageToPlayerPrevented" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.PlayerGainsLife r -> tag "PlayerGainsLife" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.PlayerLosesLife r -> tag "PlayerLosesLife" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.SelfCountersReached x -> tag "SelfCountersReached" $ Codec.encode SelfCountersReached.codec x
      TriggerCondition.SelfLastCounterRemoved kind -> tag "SelfLastCounterRemoved" $ Codec.encode counterKindCodec kind
      TriggerCondition.SpellCast x -> tag "SpellCast" $ Codec.encode SpellCast.codec x
      TriggerCondition.SelfCast -> Common.nullary "SelfCast"
      TriggerCondition.SelfHalfUnlocked n -> tag "SelfHalfUnlocked" $ Codec.encode CardName.codec n
      TriggerCondition.RoomFullyUnlocked r -> tag "RoomFullyUnlocked" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.AnyOf cs -> tag "AnyOf" $ Codec.encode (Common.list codec) cs
      TriggerCondition.SelfTurnedFaceUp -> Common.nullary "SelfTurnedFaceUp"
      TriggerCondition.PermanentTurnedFaceUp f -> tag "PermanentTurnedFaceUp" $ Codec.encode filterCodec f
      TriggerCondition.PermanentBecomesDesignated x -> tag "PermanentBecomesDesignated" $ Codec.encode PermanentBecomesDesignated.codec x
      TriggerCondition.SelfEvolves -> Common.nullary "SelfEvolves"
      TriggerCondition.AttachedCreatureMentors -> Common.nullary "AttachedCreatureMentors"
      TriggerCondition.PermanentSacrificed -> Common.nullary "PermanentSacrificed"
      TriggerCondition.SagaFinalChapterTriggers r -> tag "SagaFinalChapterTriggers" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.PlayerBecomesMonarch r -> tag "PlayerBecomesMonarch" $ Codec.encode PlayerRelation.codec r
      TriggerCondition.LoseControlOfBound s -> tag "LoseControlOfBound" $ Codec.encode SlotName.codec s
      TriggerCondition.RoomEntered r -> tag "RoomEntered" $ Codec.encode RoomIndex.codec r
