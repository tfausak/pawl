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
    [ Arm.nullary "SelfEnters" TriggerCondition.SelfEnters,
      Arm.payload "PermanentEnters" filterCodec TriggerCondition.PermanentEnters (\x -> case x of TriggerCondition.PermanentEnters y -> Just y; _ -> Nothing),
      Arm.payload "StepBegins" StepBegins.codec TriggerCondition.StepBegins (\x -> case x of TriggerCondition.StepBegins y -> Just y; _ -> Nothing),
      Arm.payload "StateIs" Condition.codec TriggerCondition.StateIs (\x -> case x of TriggerCondition.StateIs y -> Just y; _ -> Nothing),
      Arm.nullary "SelfDealsCombatDamageToPlayer" TriggerCondition.SelfDealsCombatDamageToPlayer,
      Arm.nullary "SelfIsDealtDamage" TriggerCondition.SelfIsDealtDamage,
      Arm.payload "PermanentDealsCombatDamageToPlayer" filterCodec TriggerCondition.PermanentDealsCombatDamageToPlayer (\x -> case x of TriggerCondition.PermanentDealsCombatDamageToPlayer y -> Just y; _ -> Nothing),
      Arm.nullary "CreatureDealtCombatDamageToMonarch" TriggerCondition.CreatureDealtCombatDamageToMonarch,
      Arm.nullary "OpponentLostLifeDuringYourTurn" TriggerCondition.OpponentLostLifeDuringYourTurn,
      Arm.payload "SelfAttacks" TriggerFrequency.codec TriggerCondition.SelfAttacks (\x -> case x of TriggerCondition.SelfAttacks y -> Just y; _ -> Nothing),
      Arm.payload "SelfAttacksWithAnother" filterCodec TriggerCondition.SelfAttacksWithAnother (\x -> case x of TriggerCondition.SelfAttacksWithAnother y -> Just y; _ -> Nothing),
      Arm.payload "CreatureAttacksAlone" filterCodec TriggerCondition.CreatureAttacksAlone (\x -> case x of TriggerCondition.CreatureAttacksAlone y -> Just y; _ -> Nothing),
      Arm.nullary "CreatureAttacksYou" TriggerCondition.CreatureAttacksYou,
      Arm.nullary "SelfAttacksPlayerWithMostLife" TriggerCondition.SelfAttacksPlayerWithMostLife,
      Arm.nullary "SelfBlocks" TriggerCondition.SelfBlocks,
      Arm.payload "SelfBlocksCreature" filterCodec TriggerCondition.SelfBlocksCreature (\x -> case x of TriggerCondition.SelfBlocksCreature y -> Just y; _ -> Nothing),
      Arm.payload "SelfBlocksAtLeast" Common.natural TriggerCondition.SelfBlocksAtLeast (\x -> case x of TriggerCondition.SelfBlocksAtLeast y -> Just y; _ -> Nothing),
      Arm.payload "SelfBlocksOneOrMore" filterCodec TriggerCondition.SelfBlocksOneOrMore (\x -> case x of TriggerCondition.SelfBlocksOneOrMore y -> Just y; _ -> Nothing),
      Arm.nullary "SelfBecomesBlocked" TriggerCondition.SelfBecomesBlocked,
      Arm.payload "SelfBecomesBlockedBy" filterCodec TriggerCondition.SelfBecomesBlockedBy (\x -> case x of TriggerCondition.SelfBecomesBlockedBy y -> Just y; _ -> Nothing),
      Arm.payload "SelfBecomesBlockedByOneOrMore" filterCodec TriggerCondition.SelfBecomesBlockedByOneOrMore (\x -> case x of TriggerCondition.SelfBecomesBlockedByOneOrMore y -> Just y; _ -> Nothing),
      Arm.nullary "SelfAttacksUnblocked" TriggerCondition.SelfAttacksUnblocked,
      Arm.nullary "SelfCycled" TriggerCondition.SelfCycled,
      Arm.nullary "SelfRevealedForMiracle" TriggerCondition.SelfRevealedForMiracle,
      Arm.nullary "SelfDiscarded" TriggerCondition.SelfDiscarded,
      Arm.payload "PlayerDiscards" PlayerRelation.codec TriggerCondition.PlayerDiscards (\x -> case x of TriggerCondition.PlayerDiscards y -> Just y; _ -> Nothing),
      Arm.payload "PlayerCycles" PlayerRelation.codec TriggerCondition.PlayerCycles (\x -> case x of TriggerCondition.PlayerCycles y -> Just y; _ -> Nothing),
      Arm.payload "PlayerDrawsNthCard" PlayerDrawsNthCard.codec TriggerCondition.PlayerDrawsNthCard (\x -> case x of TriggerCondition.PlayerDrawsNthCard y -> Just y; _ -> Nothing),
      Arm.nullary "SelfPutIntoGraveyardFromLibrary" TriggerCondition.SelfPutIntoGraveyardFromLibrary,
      Arm.nullary "SelfPutIntoGraveyardFromAnywhere" TriggerCondition.SelfPutIntoGraveyardFromAnywhere,
      Arm.nullary "SelfDies" TriggerCondition.SelfDies,
      Arm.payload "PermanentDies" filterCodec TriggerCondition.PermanentDies (\x -> case x of TriggerCondition.PermanentDies y -> Just y; _ -> Nothing),
      Arm.nullary "SelfLeavesTheBattlefield" TriggerCondition.SelfLeavesTheBattlefield,
      Arm.nullary "HauntedCreatureDies" TriggerCondition.HauntedCreatureDies,
      Arm.payload "SpellOrAbilityCounters" PlayerRelation.codec TriggerCondition.SpellOrAbilityCounters (\x -> case x of TriggerCondition.SpellOrAbilityCounters y -> Just y; _ -> Nothing),
      Arm.payload "DamageToPlayerPrevented" PlayerRelation.codec TriggerCondition.DamageToPlayerPrevented (\x -> case x of TriggerCondition.DamageToPlayerPrevented y -> Just y; _ -> Nothing),
      Arm.payload "PlayerGainsLife" PlayerRelation.codec TriggerCondition.PlayerGainsLife (\x -> case x of TriggerCondition.PlayerGainsLife y -> Just y; _ -> Nothing),
      Arm.payload "PlayerLosesLife" PlayerRelation.codec TriggerCondition.PlayerLosesLife (\x -> case x of TriggerCondition.PlayerLosesLife y -> Just y; _ -> Nothing),
      Arm.payload "SelfCountersReached" SelfCountersReached.codec TriggerCondition.SelfCountersReached (\x -> case x of TriggerCondition.SelfCountersReached y -> Just y; _ -> Nothing),
      Arm.payload "SelfLastCounterRemoved" counterKindCodec TriggerCondition.SelfLastCounterRemoved (\x -> case x of TriggerCondition.SelfLastCounterRemoved y -> Just y; _ -> Nothing),
      Arm.payload "SpellCast" SpellCast.codec TriggerCondition.SpellCast (\x -> case x of TriggerCondition.SpellCast y -> Just y; _ -> Nothing),
      Arm.nullary "SelfCast" TriggerCondition.SelfCast,
      Arm.payload "SelfBecomesTargeted" PlayerRelation.codec TriggerCondition.SelfBecomesTargeted (\x -> case x of TriggerCondition.SelfBecomesTargeted y -> Just y; _ -> Nothing),
      Arm.payload "SelfHalfUnlocked" CardName.codec TriggerCondition.SelfHalfUnlocked (\x -> case x of TriggerCondition.SelfHalfUnlocked y -> Just y; _ -> Nothing),
      Arm.payload "RoomFullyUnlocked" PlayerRelation.codec TriggerCondition.RoomFullyUnlocked (\x -> case x of TriggerCondition.RoomFullyUnlocked y -> Just y; _ -> Nothing),
      Arm.payload "AnyOf" (Common.list codec) TriggerCondition.AnyOf (\x -> case x of TriggerCondition.AnyOf y -> Just y; _ -> Nothing),
      Arm.nullary "SelfTurnedFaceUp" TriggerCondition.SelfTurnedFaceUp,
      Arm.payload "PermanentTurnedFaceUp" filterCodec TriggerCondition.PermanentTurnedFaceUp (\x -> case x of TriggerCondition.PermanentTurnedFaceUp y -> Just y; _ -> Nothing),
      Arm.payload "PermanentBecomesDesignated" PermanentBecomesDesignated.codec TriggerCondition.PermanentBecomesDesignated (\x -> case x of TriggerCondition.PermanentBecomesDesignated y -> Just y; _ -> Nothing),
      Arm.nullary "SelfEvolves" TriggerCondition.SelfEvolves,
      Arm.nullary "AttachedCreatureMentors" TriggerCondition.AttachedCreatureMentors,
      Arm.nullary "SelfTrains" TriggerCondition.SelfTrains,
      Arm.nullary "PermanentSacrificed" TriggerCondition.PermanentSacrificed,
      Arm.payload "SagaFinalChapterTriggers" PlayerRelation.codec TriggerCondition.SagaFinalChapterTriggers (\x -> case x of TriggerCondition.SagaFinalChapterTriggers y -> Just y; _ -> Nothing),
      Arm.payload "PlayerBecomesMonarch" PlayerRelation.codec TriggerCondition.PlayerBecomesMonarch (\x -> case x of TriggerCondition.PlayerBecomesMonarch y -> Just y; _ -> Nothing),
      Arm.payload "LoseControlOfBound" SlotName.codec TriggerCondition.LoseControlOfBound (\x -> case x of TriggerCondition.LoseControlOfBound y -> Just y; _ -> Nothing),
      Arm.payload "RoomEntered" RoomIndex.codec TriggerCondition.RoomEntered (\x -> case x of TriggerCondition.RoomEntered y -> Just y; _ -> Nothing),
      Arm.payload "PlayerScries" PlayerRelation.codec TriggerCondition.PlayerScries (\x -> case x of TriggerCondition.PlayerScries y -> Just y; _ -> Nothing),
      Arm.payload "PlayerSurveils" PlayerRelation.codec TriggerCondition.PlayerSurveils (\x -> case x of TriggerCondition.PlayerSurveils y -> Just y; _ -> Nothing),
      Arm.nullary "SelfBecomesPlotted" TriggerCondition.SelfBecomesPlotted,
      Arm.payload "PermanentExplores" filterCodec TriggerCondition.PermanentExplores (\x -> case x of TriggerCondition.PermanentExplores y -> Just y; _ -> Nothing),
      Arm.nullary "SelfExerted" TriggerCondition.SelfExerted
    ]
  where
    filterCodec = Filter.codec Keyword.codec
    counterKindCodec = CounterKind.codec Keyword.codec
