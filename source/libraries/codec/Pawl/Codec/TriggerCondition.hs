module Pawl.Codec.TriggerCondition where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TriggerCondition as TriggerCondition

toJson :: TriggerCondition.TriggerCondition -> Value.Value
toJson c = case c of
  TriggerCondition.SelfEnters -> Common.nullary "SelfEnters"
  TriggerCondition.PermanentEnters f -> Common.tagged "PermanentEnters" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.StepBegins p s -> Common.tagged "StepBegins" . Just . Common.array $ [Phase.toJson p, TurnScope.toJson s]
  TriggerCondition.StateIs c2 -> Common.tagged "StateIs" . Just $ Condition.toJson c2
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Common.nullary "SelfDealsCombatDamageToPlayer"
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> Common.tagged "PermanentDealsCombatDamageToPlayer" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Common.nullary "CreatureDealtCombatDamageToMonarch"
  TriggerCondition.OpponentLostLifeDuringYourTurn -> Common.nullary "OpponentLostLifeDuringYourTurn"
  TriggerCondition.SelfAttacks f -> Common.tagged "SelfAttacks" . Just $ TriggerFrequency.toJson f
  TriggerCondition.SelfAttacksWithAnother f -> Common.tagged "SelfAttacksWithAnother" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.CreatureAttacksAlone f -> Common.tagged "CreatureAttacksAlone" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.SelfAttacksPlayerWithMostLife -> Common.nullary "SelfAttacksPlayerWithMostLife"
  TriggerCondition.SelfBlocks -> Common.nullary "SelfBlocks"
  TriggerCondition.SelfBlocksCreature -> Common.nullary "SelfBlocksCreature"
  TriggerCondition.SelfBlocksAtLeast n -> Common.tagged "SelfBlocksAtLeast" . Just $ Common.encodeNatural n
  TriggerCondition.SelfBlocksOneOrMore f -> Common.tagged "SelfBlocksOneOrMore" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.SelfBecomesBlocked -> Common.nullary "SelfBecomesBlocked"
  TriggerCondition.SelfBecomesBlockedBy f -> Common.tagged "SelfBecomesBlockedBy" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> Common.tagged "SelfBecomesBlockedByOneOrMore" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.SelfCycled -> Common.nullary "SelfCycled"
  TriggerCondition.PlayerDiscards r -> Common.tagged "PlayerDiscards" . Just $ PlayerRelation.toJson r
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Common.nullary "SelfPutIntoGraveyardFromLibrary"
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Common.nullary "SelfPutIntoGraveyardFromAnywhere"
  TriggerCondition.SelfDies -> Common.nullary "SelfDies"
  TriggerCondition.PermanentDies f -> Common.tagged "PermanentDies" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.SelfLeavesTheBattlefield -> Common.nullary "SelfLeavesTheBattlefield"
  TriggerCondition.SpellOrAbilityCounters r -> Common.tagged "SpellOrAbilityCounters" . Just $ PlayerRelation.toJson r
  TriggerCondition.DamageToPlayerPrevented r -> Common.tagged "DamageToPlayerPrevented" . Just $ PlayerRelation.toJson r
  TriggerCondition.PlayerGainsLife r -> Common.tagged "PlayerGainsLife" . Just $ PlayerRelation.toJson r
  TriggerCondition.PlayerLosesLife r -> Common.tagged "PlayerLosesLife" . Just $ PlayerRelation.toJson r
  TriggerCondition.SelfCountersReached kind n -> Common.tagged "SelfCountersReached" . Just . Common.array $ [CounterKind.toJson Keyword.toJson kind, Common.encodeNatural n]
  TriggerCondition.SelfLastCounterRemoved kind -> Common.tagged "SelfLastCounterRemoved" . Just $ CounterKind.toJson Keyword.toJson kind
  TriggerCondition.SpellCast f s -> Common.tagged "SpellCast" . Just . Common.array $ [Filter.toJson Keyword.toJson f, TurnScope.toJson s]
  TriggerCondition.SelfCast -> Common.nullary "SelfCast"
  TriggerCondition.SelfHalfUnlocked n -> Common.tagged "SelfHalfUnlocked" . Just $ CardName.toJson n
  TriggerCondition.RoomFullyUnlocked r -> Common.tagged "RoomFullyUnlocked" . Just $ PlayerRelation.toJson r
  -- RECURSIVE, and the only condition that is: an AnyOf holds conditions, so both
  -- directions of this codec call themselves.
  TriggerCondition.AnyOf cs -> Common.tagged "AnyOf" . Just . Common.array $ fmap toJson cs
  TriggerCondition.SelfTurnedFaceUp -> Common.nullary "SelfTurnedFaceUp"
  TriggerCondition.PermanentTurnedFaceUp f -> Common.tagged "PermanentTurnedFaceUp" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.PermanentBecomesDesignated d f -> Common.tagged "PermanentBecomesDesignated" . Just . Common.array $ [Designation.toJson d, Filter.toJson Keyword.toJson f]
  TriggerCondition.SelfEvolves -> Common.nullary "SelfEvolves"
  TriggerCondition.AttachedCreatureMentors -> Common.nullary "AttachedCreatureMentors"
  TriggerCondition.PermanentSacrificed -> Common.nullary "PermanentSacrificed"
  TriggerCondition.SagaFinalChapterTriggers r -> Common.tagged "SagaFinalChapterTriggers" . Just $ PlayerRelation.toJson r
  TriggerCondition.PlayerBecomesMonarch r -> Common.tagged "PlayerBecomesMonarch" . Just $ PlayerRelation.toJson r
  TriggerCondition.LoseControlOfBound s -> Common.tagged "LoseControlOfBound" . Just $ SlotName.toJson s

fromJson :: Value.Value -> Either Text.Text TriggerCondition.TriggerCondition
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("PermanentEnters", Just v) -> TriggerCondition.PermanentEnters <$> Filter.fromJson Keyword.fromJson v
    ("StepBegins", Just (Value.Array (Array.MkArray [p, s]))) -> TriggerCondition.StepBegins <$> Phase.fromJson p <*> TurnScope.fromJson s
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> Condition.fromJson v
    ("SelfDealsCombatDamageToPlayer", _) -> Right TriggerCondition.SelfDealsCombatDamageToPlayer
    ("PermanentDealsCombatDamageToPlayer", Just v) -> TriggerCondition.PermanentDealsCombatDamageToPlayer <$> Filter.fromJson Keyword.fromJson v
    ("CreatureDealtCombatDamageToMonarch", _) -> Right TriggerCondition.CreatureDealtCombatDamageToMonarch
    ("OpponentLostLifeDuringYourTurn", _) -> Right TriggerCondition.OpponentLostLifeDuringYourTurn
    ("SelfAttacks", Just v) -> TriggerCondition.SelfAttacks <$> TriggerFrequency.fromJson v
    ("SelfAttacksWithAnother", Just v) -> TriggerCondition.SelfAttacksWithAnother <$> Filter.fromJson Keyword.fromJson v
    ("CreatureAttacksAlone", Just v) -> TriggerCondition.CreatureAttacksAlone <$> Filter.fromJson Keyword.fromJson v
    ("SelfAttacksPlayerWithMostLife", _) -> Right TriggerCondition.SelfAttacksPlayerWithMostLife
    ("SelfBlocks", _) -> Right TriggerCondition.SelfBlocks
    ("SelfBlocksCreature", _) -> Right TriggerCondition.SelfBlocksCreature
    ("SelfBlocksAtLeast", Just v) -> TriggerCondition.SelfBlocksAtLeast <$> Common.decodeNatural v
    ("SelfBlocksOneOrMore", Just v) -> TriggerCondition.SelfBlocksOneOrMore <$> Filter.fromJson Keyword.fromJson v
    ("SelfBecomesBlocked", _) -> Right TriggerCondition.SelfBecomesBlocked
    ("SelfBecomesBlockedBy", Just v) -> TriggerCondition.SelfBecomesBlockedBy <$> Filter.fromJson Keyword.fromJson v
    ("SelfBecomesBlockedByOneOrMore", Just v) -> TriggerCondition.SelfBecomesBlockedByOneOrMore <$> Filter.fromJson Keyword.fromJson v
    ("SelfCycled", _) -> Right TriggerCondition.SelfCycled
    ("PlayerDiscards", Just v) -> TriggerCondition.PlayerDiscards <$> PlayerRelation.fromJson v
    ("SelfPutIntoGraveyardFromLibrary", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromLibrary
    ("SelfPutIntoGraveyardFromAnywhere", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromAnywhere
    ("SelfDies", _) -> Right TriggerCondition.SelfDies
    ("PermanentDies", Just v) -> TriggerCondition.PermanentDies <$> Filter.fromJson Keyword.fromJson v
    ("SelfLeavesTheBattlefield", _) -> Right TriggerCondition.SelfLeavesTheBattlefield
    ("SpellOrAbilityCounters", Just v) -> TriggerCondition.SpellOrAbilityCounters <$> PlayerRelation.fromJson v
    ("DamageToPlayerPrevented", Just v) -> TriggerCondition.DamageToPlayerPrevented <$> PlayerRelation.fromJson v
    ("PlayerGainsLife", Just v) -> TriggerCondition.PlayerGainsLife <$> PlayerRelation.fromJson v
    ("PlayerLosesLife", Just v) -> TriggerCondition.PlayerLosesLife <$> PlayerRelation.fromJson v
    ("SelfCountersReached", Just (Value.Array (Array.MkArray [kind, n]))) -> TriggerCondition.SelfCountersReached <$> CounterKind.fromJson Keyword.fromJson kind <*> Common.decodeNatural n
    ("SelfLastCounterRemoved", Just v) -> TriggerCondition.SelfLastCounterRemoved <$> CounterKind.fromJson Keyword.fromJson v
    ("SpellCast", Just (Value.Array (Array.MkArray [f, s]))) -> TriggerCondition.SpellCast <$> Filter.fromJson Keyword.fromJson f <*> TurnScope.fromJson s
    ("SelfCast", _) -> Right TriggerCondition.SelfCast
    ("SelfHalfUnlocked", Just v) -> TriggerCondition.SelfHalfUnlocked <$> CardName.fromJson v
    ("RoomFullyUnlocked", Just v) -> TriggerCondition.RoomFullyUnlocked <$> PlayerRelation.fromJson v
    ("AnyOf", Just (Value.Array (Array.MkArray cs))) -> TriggerCondition.AnyOf <$> traverse fromJson cs
    ("SelfTurnedFaceUp", _) -> Right TriggerCondition.SelfTurnedFaceUp
    ("PermanentTurnedFaceUp", Just v) -> TriggerCondition.PermanentTurnedFaceUp <$> Filter.fromJson Keyword.fromJson v
    ("PermanentBecomesDesignated", Just (Value.Array (Array.MkArray [d, f]))) ->
      TriggerCondition.PermanentBecomesDesignated <$> Designation.fromJson d <*> Filter.fromJson Keyword.fromJson f
    ("SelfEvolves", _) -> Right TriggerCondition.SelfEvolves
    ("AttachedCreatureMentors", _) -> Right TriggerCondition.AttachedCreatureMentors
    ("PermanentSacrificed", _) -> Right TriggerCondition.PermanentSacrificed
    ("SagaFinalChapterTriggers", Just v) -> TriggerCondition.SagaFinalChapterTriggers <$> PlayerRelation.fromJson v
    ("PlayerBecomesMonarch", Just v) -> TriggerCondition.PlayerBecomesMonarch <$> PlayerRelation.fromJson v
    ("LoseControlOfBound", Just v) -> TriggerCondition.LoseControlOfBound <$> SlotName.fromJson v
    _ -> Left . Text.pack $ "unknown TriggerCondition: " <> t
